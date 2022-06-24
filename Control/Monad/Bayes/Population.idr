module Control.Monad.Bayes.Population

import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Interface
import Data.List

namespace ListT
  record ListT (m : Type -> Type) (a : Type) where
    constructor MkListT 
    runListT : m (List a)

  Functor m => Functor (ListT m) where
    map f (MkListT mas) = MkListT (map (map f) mas) 

  Applicative m => Applicative (ListT m) where
    pure x = MkListT (pure [x])
    (MkListT mf) <*> (MkListT mx) = MkListT ((map (<*>) mf) <*> mx) 

  Monad m => Monad (ListT m) where
    (MkListT mas) >>= f = MkListT $ do
      as <- mas
      bss <- sequence (map (runListT . f) as)
      pure (concat bss)

||| A collection of weighted samples, or particles.
Population : (m : Type -> Type) -> (a : Type) -> Type
Population m = Weighted (ListT m)

||| Explicit representation of the weighted sample with weights in the log domain.
runPopulation : Population m a -> m (List (Log Double, a))
runPopulation = runListT . runWeighted

||| Explicit representation of the weighted sample.
explicitPopulation : Functor m => Population m a -> m (List (Double, a))
explicitPopulation = map (map (\(log_p, a) => (exp (ln log_p), a))) . runPopulation

||| Initialize 'Population' with a concrete weighted sample.
fromWeightedList : Monad m => m (List (Log Double, a)) -> Population m a
fromWeightedList = withWeight . MkListT

||| Applies a transformation to the inner monad.
hoist :
  Monad m2 =>
  (forall x. m1 x -> m2 x) ->
  Population m1 a ->
  Population m2 a
hoist f = fromWeightedList . f . runPopulation

||| Increase the sample size by a given factor.
||| The weights are adjusted such that their sum is preserved. It is therefore 
||| safe to use 'spawn' in arbitrary places in the program without introducing bias.
spawn : Monad m => Nat -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n (Exp (1.0 / cast n), ()) 
-- | TODO: need to check if numeric values "5 : Log Double" means "Exp 5" or "Exp (log 5)" in Haskell version

resampleGeneric :
  MonadSample m =>
  -- | resampler
  (forall k. Vect k Double -> m (List (Fin k))) ->
  Population m a ->
  Population m a
resampleGeneric resampler pop = fromWeightedList $ do
  particles <- runPopulation pop
  let (log_ps, xs) : (Vect (length particles) (Log Double), Vect (length particles) a) = unzip (fromList particles)
      n = length xs
      z = Numeric.Log.sum log_ps
  if z > 0
    then do
      let weights = (map (exp . ln . (/ z)) log_ps)
      ancestors <- resampler weights
      let offsprings = map (\idx => index idx xs) ancestors
      pure $ map (z / cast n, ) offsprings
    else
      pure particles

||| Separate the sum of weights into the 'Weighted' transformer.
||| Weights are normalized after this operation.
extractEvidence :
  Monad m =>
  Population m a ->
  Population (Weighted m) a
extractEvidence pop = fromWeightedList $ do
  particles <- lift $ runPopulation pop -- List (Log Double, a)
  let (ps, xs) = unzip particles
      z = Numeric.Log.sum ps
      ws = map (if z > 0 then (/ z) else const (Exp $ 1.0 / cast (length ps))) ps
  score z
  pure $ zip ws xs 

-- | Push the evidence estimator as a score to the transformed monad.
-- Weights are normalized after this operation.
pushEvidence :
  MonadCond m =>
  Population m a ->
  Population m a
pushEvidence = hoist applyWeight . extractEvidence

||| A properly weighted single sample, that is one picked at random according
||| to the weights, with the sum of all weights.
proper :
  (MonadSample m) =>
  Population m a ->
  Weighted m a
proper pop = do
  particles <- runPopulation $ extractEvidence pop
  let (ps_vec, xs_vec) = unzip (fromList particles)
  idx <- the (Weighted m (Fin (length particles))) (logCategorical ps_vec)
  pure (index idx xs_vec)

||| Model evidence estimator, also known as pseudo-marginal likelihood.
evidence : (Monad m) => Population m a -> m (Log Double)
evidence = extractWeight . runPopulation . extractEvidence

||| Picks one point from the population and uses model evidence as a 'score' in the transformed monad.
||| This way a single sample can be selected from a population without introducing bias.
collapse :
  (MonadInfer m) =>
  Population m a ->
  m a
collapse = applyWeight . proper

||| Applies a random transformation to a population.
mapPopulation :
  (Monad m) =>
  (List (Log Double, a) -> m (List (Log Double, a))) ->
  Population m a ->
  Population m a
mapPopulation f m = fromWeightedList $ runPopulation m >>= f

||| Normalizes the weights in the population so that their sum is 1.
||| This transformation introduces bias.
normalize : Monad m => Population m a -> Population m a
normalize pop = hoist {m1 = Weighted m} {m2 = m} prior (extractEvidence pop)

||| Population average of a function, computed using unnormalized weights.
popAvg : (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(w, x) => f x * w) xs
  pure (Prelude.sum ys)

||| Combine a population of populations into a single population.
flatten : Monad m => Population (Population m) a -> Population m a
flatten nestedPop = withWeight $ MkListT t
  where
    f : List (Log Double, List (Log Double, a)) -> List (Log Double, a)
    f d = do
      (p, x) <- d
      (q, y) <- x
      pure (p * q, y)

    t : m (List (Log Double, a))
    t = f <$> (runPopulation . runPopulation) nestedPop

