module Control.Monad.Bayes.Sequential

import Control.Monad.Trans
import Control.Monad.Bayes.Interface

public export
data Lift : (m : Type -> Type) -> Type -> Type where
  MkLift : m a -> Lift m a

-- | 
public export
data Seq : (m : Type -> Type) -> (a : Type) -> Type where
  L : a -> Seq m a
  R : (op : m x) -> (k : x -> Seq m a) -> Seq m a

export
implementation Functor (Seq m) where
  map f (L a)    = L (f a)
  map f (R op k) = R op (map f . k)

export
implementation Applicative (Seq m) where
  pure = L
  R op k <*> p = R op (\x => k x <*> p) 
  L f    <*> p = map f p

export
implementation Monad (Seq m) where
  R op k >>= f = R op ( assert_total (>>= f) . k)
  L x   >>= f  = f x

export
MonadTrans Seq where
-- lift : Monad m => m a -> Seq m a
   lift mx = R mx (\x => L x) 

suspend : Monad m => Seq m ()
suspend = lift (pure ())

advance : Monad m => Seq m a -> (Seq m a)
advance (L a) = L a
advance (R mx k) = (lift mx) >>= k

finish : Monad m => Seq m a -> m a
finish (L a) = pure a
finish (R mx k) = mx >>= (finish . k)

finished : Monad m => Seq m a -> m Bool
finished (L a) = pure True
finished   _   = pure False

MonadSample m => MonadSample (Seq m) where
  random = lift random

MonadCond m => MonadCond (Seq m) where
  score w = lift (score w) >> suspend

hoist : (Monad m, Monad n) => (forall x. m x -> n x) -> Seq m a -> Seq n a
hoist f (L a) = (L a)  
hoist f (R mx k) = R (f mx) (hoist f . k)