module Control.Monad.Bayes.Traced.Basic

import public Data.Vect
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Traced.Common
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Free
import Control.Monad.Free

||| A tracing monad where only a subset of random choices are traced.
||| The random choices that are not to be traced should be lifted from the transformed monad.
public export
record Traced (m : Type -> Type) (a : Type) where 
  constructor MkTraced
  model     : Weighted (FreeSampler Identity) a
  traceDist : m (Trace a)

export
Monad m => Functor (Traced m) where
  map f (MkTraced m d) = MkTraced (map f m) (map (map f) d)

export
Monad m => Applicative (Traced m) where
  pure x = MkTraced (pure x) (pure (pure x))
  (MkTraced mf df) <*> (MkTraced mx dx) = MkTraced (mf <*> mx) ((map (<*>) df) <*> dx) 

export
Monad m => Monad (Traced m) where
  (MkTraced mx dx) >>= f = MkTraced my dy
    where
      my : Weighted (FreeSampler Identity) b
      my = mx >>= (model . f)
      dy : m (Trace b)
      dy = dx `bind` (traceDist . f)

export
MonadSample m => MonadSample (Traced m) where
  random = MkTraced random (map singleton random)

export
MonadCond m => MonadCond (Traced m) where
  score w = MkTraced (score w) (score w >> pure (scored w))
  
export
MonadInfer m => MonadInfer (Traced m) where

export
hoistT : (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (MkTraced m d) = MkTraced m (f d)

||| Discard the trace and supporting infrastructure.
export
marginal : Monad m => Traced m a -> m a
marginal (MkTraced _ d) = map output d

||| A single step of the Trace Metropolis-Hastings algorithm.
public export
mhStep : MonadSample m => Traced m a -> Traced m a
mhStep (MkTraced m d) = MkTraced m (d >>= mhTrans' m)

||| Full run of the Trace Metropolis-Hastings algorithm with a specified
||| number of steps. 
public export
mh : MonadSample m => (n : Nat) -> Traced m a -> m (Vect (S n) a)
mh n (MkTraced mod d) = map (map output) (f n)
  where
    f : (n : Nat) -> m (Vect (S n) (Trace a))
    f Z     = map (:: []) d
    f (S k) = do
          (x :: xs) <- f k
          y <- mhTrans' mod x
          pure (y :: x :: xs)