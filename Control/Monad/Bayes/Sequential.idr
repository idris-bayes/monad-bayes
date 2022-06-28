module Control.Monad.Bayes.Sequential

import Data.List
import Control.Monad.Trans
import Control.Monad.Bayes.Interface


||| Represents a computation that can be suspended at certain points.
-- The intermediate monadic effects can be extracted, which is particularly
-- useful for implementation of Sequential Monte Carlo related methods.
-- All the probabilistic effects are lifted from the transformed monad, but
-- also `suspend` is inserted after each `factor`.
public export
data Sequential : (m : Type -> Type) -> (a : Type) -> Type where
  L : a -> Sequential m a
  R : (op : m x) -> (k : x -> Sequential m a) -> Sequential m a

export
implementation Functor (Sequential m) where
  map f (L a)    = L (f a)
  map f (R op k) = R op (map f . k)

export
implementation Applicative (Sequential m) where
  pure = L
  R op k <*> p = R op (\x => k x <*> p) 
  L f    <*> p = map f p

export
implementation Monad (Sequential m) where
  R op k >>= f = R op ( assert_total (>>= f) . k)
  L x   >>= f  = f x

export
MonadTrans Sequential where
-- lift : Monad m => m a -> Sequential m a
   lift mx = R mx (\x => L x) 

||| A point where the computation is paused.
suspend : Monad m => Sequential m ()
suspend = lift (pure ())

||| Remove the remaining suspension points.
finish : Monad m => Sequential m a -> m a
finish (L a) = pure a
finish (R mx k) = mx >>= (finish . k)

||| Execute to the next suspension point.
-- If the computation is finished, do nothing.
advance : Monad m => Sequential m a -> Sequential m a
advance (L a) = L a
advance (R mx k) = (lift mx) >>= k

||| Return True if no more suspension points remain.
finished : Monad m => Sequential m a -> m Bool
finished (L a) = pure True
finished   _   = pure False

MonadSample m => MonadSample (Sequential m) where
  random      = lift random
  bernoulli   = lift . bernoulli
  categorical = lift . categorical

||| Execution is 'suspend'ed after each 'score'.
MonadCond m => MonadCond (Sequential m) where
  score w = lift (score w) >> suspend

MonadInfer m => MonadInfer (Sequential m)

||| Transform the inner monad.
-- This operation only applies to computation up to the first suspension.
export
hoistFirst : Monad m => (forall x. m x -> m x) -> Sequential m a -> Sequential m a
hoistFirst f (L a)    = L a 
hoistFirst f (R mx k) = R (f mx) k

||| Transform the inner monad.
-- The transformation is applied recursively through all the suspension points.
export
hoist : (Monad m, Monad n) => (forall x. m x -> n x) -> Sequential m a -> Sequential n a
hoist f (L a) = (L a)  
hoist f (R mx k) = R (f mx) (hoist f . k)

||| Apply a function a given number of times.
export
composeCopies : Nat -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (List.replicate k f)

||| Sequential importance sampling.
-- Applies a given transformation after each time step.
export
sis : Monad m
  => (forall x. m x -> m x) -- | Transformation
  -> Nat                    -- | Number of time steps
  -> Sequential m a
  -> m a
sis f k = finish . composeCopies k (advance . hoistFirst f)