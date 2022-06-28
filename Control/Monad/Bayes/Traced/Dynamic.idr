module Control.Monad.Bayes.Traced.Dynamic

import public Data.Vect
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Traced.Common
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Free
import Control.Monad.Free

||| A tracing monad where only a subset of random choices are traced and this subset can be adjusted dynamically.
public export
record Traced (m : Type -> Type) (a : Type) where 
  constructor MkTraced
  runTraced : m (Weighted (FreeSampler m) a, Trace a)

export
pushM : Monad m => m (Weighted (FreeSampler m) a) -> Weighted (FreeSampler m) a
pushM = join . lift . lift

export
Monad m => Functor (Traced m) where
  map f (MkTraced c) = MkTraced $ do
    (m, t) <- c
    let m' = map f m
        t' = map f t
    pure (m', t')

export
Monad m => Applicative (Traced m) where
  pure x = MkTraced $ pure (pure x, pure x)
  (MkTraced cf) <*> (MkTraced cx) = MkTraced $ do
    (mf, tf) <- cf
    (mx, tx) <- cx
    pure (mf <*> mx, tf <*> tx)

export
Monad m => Monad (Traced m) where
  (MkTraced cx) >>= f = MkTraced $ do
    (mx, tx) <- cx
    let m = mx >>= pushM . map fst . runTraced . f
    t <- pure tx `bind` (map snd . runTraced . f)
    pure (m, t)

export
MonadTrans Traced where
  lift m = MkTraced $ map (\a => (lift $ lift m, pure a)) m

export
MonadSample m => MonadSample (Traced m) where
  random = MkTraced $ map (\r => (random, singleton r)) random

export
MonadCond m => MonadCond (Traced m) where
  score w = MkTraced $ map (score w,) (score w >> pure (scored w))

export
MonadInfer m => MonadInfer (Traced m) where

export
hoistT : (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (MkTraced c) = MkTraced (f c)

||| Discard the trace and supporting infrastructure.
export
marginal : Monad m => Traced m a -> m a
marginal (MkTraced c) = map (output . snd) c

||| Freeze all traced random choices to their current values and stop tracing them.
export
freeze : Monad m => Traced m a -> Traced m a
freeze (MkTraced c) = MkTraced $ do
  (_, t) <- c
  let x = t.output
  pure (pure x, pure x)

||| A single step of the Trace Metropolis-Hastings algorithm.
export
mhStep : MonadSample m => Traced m a -> Traced m a
mhStep (MkTraced c) = MkTraced $ do
  (m, t) <- c
  t' <- mhTrans m t
  pure (m, t')

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
public export
mh : MonadSample m => (n : Nat) -> Traced m a -> m (Vect (S n) a)
mh n (MkTraced c) = do
  (mod, tr) <- c
  let f : (n : Nat) -> m (Vect (S n) (Trace a))
      f Z     = pure [tr]
      f (S k) = do  (x :: xs) <- f k
                    y <- mhTrans mod x
                    pure (y :: x :: xs)
  map (map output) (f n)
