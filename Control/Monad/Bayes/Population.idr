module Control.Monad.Bayes.Population

import public Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Interface
import Control.Monad.Trans
import Data.List

||| List transformer
public export
record ListT (m : Type -> Type) (a : Type) where
  constructor MkListT 
  runListT : m (List a)

export
Functor m => Functor (ListT m) where
  map f (MkListT mas) = MkListT (map (map f) mas) 

export
Applicative m => Applicative (ListT m) where
  pure x = MkListT (pure [x])
  (MkListT mf) <*> (MkListT mx) = MkListT ((map (<*>) mf) <*> mx) 

export
Monad m => Monad (ListT m) where
  (MkListT mas) >>= f = MkListT $ do
    as <- mas
    bss <- sequence (map (runListT . f) as)
    pure (concat bss)

export
MonadTrans ListT where
  lift = MkListT . map List.singleton

||| A collection of weighted samples, or particles.
public export
Population : (m : Type -> Type) -> (a : Type) -> Type
Population m = Weighted (ListT m)

export
MonadTrans Population where
  lift = lift . lift

export
MonadSample m => MonadSample (Population m) where
  random = (lift . lift) random

export
MonadCond m => MonadCond (Population m) where
  score w = (lift . lift) (score w)

export
MonadSample m => MonadInfer (Population m) where

||| Explicit representation of the weighted sample with weights in the log domain.
export
runPopulation : Population m a -> m (List (Log Double, a))
runPopulation = runListT . runWeighted

||| Explicit representation of the weighted sample.
export
explicitPopulation : Functor m => Population m a -> m (List (Double, a))
explicitPopulation = map (map (\(log_p, a) => (exp (ln log_p), a))) . runPopulation

||| Initialize 'Population' with a concrete weighted sample.
export
fromWeightedList : Monad m => m (List (Log Double, a)) -> Population m a
fromWeightedList = withWeight . MkListT

||| Applies a transformation to the inner monad.
export
hoist :
  Monad m2 =>
  (forall x. m1 x -> m2 x) ->
  Population m1 a ->
  Population m2 a
hoist f = fromWeightedList . f . runPopulation

||| Increase the sample size by a given factor.
||| The weights are adjusted such that their sum is preserved. It is therefore 
||| safe to use 'spawn' in arbitrary places in the program without introducing bias.
export
spawn : (isMonad : Monad m) => Nat -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n (Exp (1.0 / the (Double) (cast n)), ()) 

export
resampleGeneric :
  MonadSample m => 
  -- | resampler
  ({k : Nat} -> Vect k Double -> m (List (Fin k))) ->
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

||| Systematic sampler.
export
systematic : {n : Nat} -> Double -> Vect n Double -> List (Fin n)
systematic {n = Z}   u Nil = Nil
systematic {n = S k} u (p :: ps) =
  let     prob : Fin (S k) -> Double
          prob idx = index idx (p :: ps)

          inc : Double
          inc = 1 / cast (S k)

          bounded_succ : {x : Nat} -> Fin x -> Fin x
          bounded_succ k = case strengthen (FS k) of Just sx => sx
                                                     Nothing => k

          bounded_unsucc : Fin x -> Fin x
          bounded_unsucc FZ = FZ
          bounded_unsucc (FS k) = weaken k

          f : Fin (S k) -> Double -> Fin (S k) -> Double -> List (Fin (S k)) -> List (Fin (S k))
          f i v j q acc = 
            if finToNat i == k then acc else
            if v < q
              then f (bounded_succ i) (v + inc) j q (bounded_unsucc j :: acc)
              else f i v (bounded_succ j) (q + prob j) acc
          
  in      f FZ (u / cast (S k)) FZ 0.0 []

||| Resample the population using the underlying monad and a systematic resampling scheme.
||| The total weight is preserved.
export
resampleSystematic :
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleSystematic = resampleGeneric (\ps => (`systematic` ps) <$> random)

||| Multinomial sampler.  Sample from \(0, \ldots, n - 1\) \(n\)
||| times drawn at random according to the weights where \(n\) is the
||| length of vector of weights.
export
multinomial : MonadSample m => {n : Nat} -> Vect n Double -> m (List (Fin n))
multinomial ps = sequence $ replicate n (categorical ps)

||| Resample the population using the underlying monad and a multinomial resampling scheme.
||| The total weight is preserved.
export
resampleMultinomial :
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleMultinomial = resampleGeneric multinomial

||| Separate the sum of weights into the 'Weighted' transformer.
||| Weights are normalized after this operation.
export
extractEvidence :
  Monad m =>
  Population m a ->
  Population (Weighted m) a
extractEvidence pop = fromWeightedList $ do
  particles <- lift $ runPopulation pop -- List (Log Double, a)
  let (log_ps, xs) = unzip particles
      z = Numeric.Log.sum log_ps
      ws = map (if z > 0 then (/ z) else const (Exp $ 1.0 / cast (length log_ps))) log_ps
  score z
  pure $ zip ws xs 

||| Push the evidence estimator as a score to the transformed monad.
||| Weights are normalized after this operation.
export
pushEvidence :
  MonadCond m =>
  Population m a ->
  Population m a
pushEvidence = hoist applyWeight . extractEvidence

||| A properly weighted single sample, that is one picked at random according
||| to the weights, with the sum of all weights.
export
proper :
  (MonadSample m) =>
  Population m a ->
  Weighted m a
proper pop = do
  particles <- runPopulation $ extractEvidence pop
  let (log_ps_vec, xs_vec) = unzip (fromList particles)
  idx <- the (Weighted m (Fin (length particles))) (logCategorical log_ps_vec)
  pure (index idx xs_vec)

||| Model evidence estimator, also known as pseudo-marginal likelihood.
export
evidence : (Monad m) => Population m a -> m (Log Double)
evidence = extractWeight . runPopulation . extractEvidence

||| Picks one point from the population and uses model evidence as a 'score' in the transformed monad.
||| This way a single sample can be selected from a population without introducing bias.
export
collapse :
  (MonadInfer m) =>
  Population m a ->
  m a
collapse = applyWeight . proper

||| Applies a random transformation to a population.
export
mapPopulation :
  (Monad m) =>
  (List (Log Double, a) -> m (List (Log Double, a))) ->
  Population m a ->
  Population m a
mapPopulation f m = fromWeightedList $ runPopulation m >>= f

||| Normalizes the weights in the population so that their sum is 1.
||| This transformation introduces bias.
export
normalize : Monad m => Population m a -> Population m a
normalize pop = hoist {m1 = Weighted m} {m2 = m} prior (extractEvidence pop)

||| Population average of a function, computed using unnormalized weights.
export
popAvg : (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(w, x) => f x * w) xs
  pure (Prelude.sum ys)

||| Combine a population of populations into a single population.
export
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

