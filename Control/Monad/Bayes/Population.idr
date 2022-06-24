module Control.Monad.Bayes.Population

import Control.Monad.Bayes.Weighted
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


-- systematic : Double -> List Double -> List Nat
-- systematic u ps = f 0 (u / fromIntegral n) 0 0 []
--   where
--     prob : (i : Nat) ->  InBounds i ps => Double
--     prob i = index i ps 
--     n : Nat 
--     n = length ps
--     inc : Double
--     inc = 1.0 / cast n

--     unsucc : Nat -> Nat
--     unsucc (S k) = k
--     unsucc Z     = Z

--     f : (i : Nat) -> InBounds (S i) ps => Double -> (j : Nat) -> Double -> (acc : List Nat) -> List Nat
--     f i v j q acc =
--       if i == n then acc
--       else if v < q then f (S i) (v + inc) j q (unsucc j :: acc)
--       else f i v (j + 1) (q + prob j) acc

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

||| Applies a transformation to the inner monad.
hoist :
  Monad n =>
  (forall x. m x -> n x) ->
  Population m a ->
  Population n a
hoist f = fromWeightedList . f . runPopulation
