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

||| Increase the sample size by a given factor.
||| The weights are adjusted such that their sum is preserved. It is therefore 
||| safe to use 'spawn' in arbitrary places in the program without introducing bias.
spawn : Monad m => Nat -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n (Exp (1.0 / cast n), ()) 
-- | TODO: need to check if numeric values "5 : Log Double" means "Exp 5" or "Exp (log 5)" in Haskell version

extractEvidence :
  Monad m =>
  Population m a ->
  Population (Weighted m) a
extractEvidence m = fromWeightedList $ do
  pop <- lift $ runPopulation m -- List (Log Double, a)
  let (ps, xs) = unzip pop
      z = sum ps
      ws = map (if z > 0 then (/ z) else const (Exp $ 1.0 / cast (length ps))) ps
  score z
  pure $ zip ws xs 

||| Applies a transformation to the inner monad.
hoist :
  Monad m2 =>
  (forall x. m1 x -> m2 x) ->
  Population m1 a ->
  Population m2 a
hoist f = fromWeightedList . f . runPopulation

||| Normalizes the weights in the population so that their sum is 1.
||| This transformation introduces bias.
normalize : Monad m => Population m a -> Population m a
normalize pop = hoist {m1 = Weighted m} {m2 = m} prior (extractEvidence pop)

||| Population average of a function, computed using unnormalized weights.
popAvg : (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(w, x) => f x * w) xs
  pure (sum ys)

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

