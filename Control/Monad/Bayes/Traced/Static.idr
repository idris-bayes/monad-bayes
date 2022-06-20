module Control.Monad.Bayes.Traced.Static

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Traced.Common
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Free


record Traced (m : Type -> Type) (a : Type) where 
  constructor MkTraced
  model     : Weighted (FreeSampler m) a
  traceDist : m (Trace a)

liftA2 : Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa = (<*>) (map f fa)

Monad m => Functor (Traced m) where
  map f (MkTraced m d) = MkTraced (map f m) (map (map f) d)

Monad m => Applicative (Traced m) where
  pure x = MkTraced (pure x) (pure (pure x))
  (MkTraced mf df) <*> (MkTraced mx dx) = MkTraced (mf <*> mx) (liftA2 (<*>) df dx) 

Monad m => Monad (Traced m) where
  (MkTraced mx dx) >>= f = MkTraced my dy
    where
      my : Weighted (FreeSampler m) b
      my =  mx >>= (model . f)
      dy : m (Trace b)
      dy = dx `bind` (traceDist . f)

MonadTrans Traced where
  lift m = MkTraced (lift $ lift m) (map pure m)

MonadSample m => MonadSample (Traced m) where
  random = MkTraced ?rand (map singleton random)
  -- random = MkTraced random (map singleton random)

MonadCond m => MonadCond (Traced m) where
  score w = MkTraced (score w) (score w >> pure (scored w))
