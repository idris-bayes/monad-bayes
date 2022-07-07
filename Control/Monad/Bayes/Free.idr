module Control.Monad.Bayes.Free

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Bayes.Interface
import Control.Monad.Free
import public Control.Monad.Trans.Free.Church

||| Random sampling functor
public export
data SamF a = Random (Double -> a)

public export
Functor SamF where
  map f (Random k) = Random (f . k)

||| Free monad transformer over random sampling.
||| Uses the Church-encoded version of the free monad for efficiency.
public export
FreeSampler : (m : Type -> Type) -> (a : Type) -> Type
FreeSampler = FT SamF

export
(Monad m, MonadFree SamF (FreeSampler m)) => MonadSample (FreeSampler m) where
  random = liftF $ Random id

||| Hoist 'FreeSampler' through a monad transform.
export
hoist : (Monad m, Monad n) => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoist f m = hoistFT f m

||| Execute random sampling in the transformed monad.
export
interpret : MonadSample m => FreeSampler m a -> m a
interpret c = iterT (\(Random k) => random >>= k) c

||| Execute computation with supplied values for random choices.
export
withRandomness : Monad m => List Double -> FreeSampler m a -> m a
withRandomness randomness = evalStateT randomness . iterTM f
  where f : MonadState (List Double) n => SamF (n b) -> n b
        f (Random k) = do
          xs <- the (n (List Double)) get
          case xs of
            []      => assert_total $ idris_crash ("withRandomness: randomness too short!")
            y :: ys => put ys >> k y

MonadTrans (\m => StateT (List Double) (WriterT (List Double) m)) where
  lift = lift . lift

||| Execute computation with supplied values for a subset of random choices.
||| Return the output value and a record of all random choices used, whether
||| taken as input or drawn using the transformed monad.
public export
withPartialRandomness : MonadSample m => List Double -> FreeSampler m a -> m (a, List Double)
withPartialRandomness randomness k = 
  runWriterT $ 
    evalStateT randomness $ 
      iterTM {t = \m => StateT (List Double) (WriterT (List Double) m) } f k
  where f : (MonadSample n, MonadWriter (List Double) n, MonadState (List Double) n) => SamF (n a) -> n a
        f (Random k) = do
          xs <- the (n (List Double)) get
          x <- case xs of
            []      => random
            y :: ys => put ys >> pure y
          tell [x]
          k x

||| Like `withPartialRandomness`, but use an arbitrary sampling monad.
public export
runWith : MonadSample m => List Double -> FreeSampler Identity a -> m (a, List Double)
runWith randomness = withPartialRandomness randomness . hoist {n=m} (pure . runIdentity)
