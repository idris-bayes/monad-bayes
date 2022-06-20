module Control.Monad.Bayes.Free

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Bayes.Interface
import Control.Monad.Free
import Control.Monad.Free.Church
--import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church

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

--record FreeSampler m a where
  --constructor MkFreeSampler
  --runFreeSampler : FT SamF m a

-- TODO: check if this is needed, since we're just aliasing FT
--{m : _} -> Monad m => MonadFree SamF (FreeSampler m) where
  --wrap (Random x) = MkFT $ ?a

export
(Monad m, MonadFree SamF (FreeSampler m)) => MonadSample (FreeSampler m) where
  random = MkFT (?h (Random id))

||| Hoist 'FreeSampler' through a monad transform.
export
hoist : {n : _} -> (Monad m, Monad n) => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
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
            []      => ?randomness_too_short  -- ERROR
            y :: ys => put ys >> k y

||| Execute computation with supplied values for a subset of random choices.
||| Return the output value and a record of all random choices used, whether
||| taken as input or drawn using the transformed monad.
withPartialRandomness' : (MonadWriter (List Double) m, Monad m) => List Double -> FreeSampler m a -> m a
withPartialRandomness' randomness k = evalStateT randomness $ iterTM f k
  where f : (MonadWriter (List Double) n, MonadState (List Double) n) => SamF (n a) -> n a
        f (Random k) = do
          xs <- the (n (List Double)) get
          x <- case xs of
            []      => ?randomness_too_short1  -- ERROR
            y :: ys => put ys >> pure y
          tell [x]
          k x

export
withPartialRandomness : (Monad m, MonadWriter (List Double) m) => List Double -> FreeSampler m a -> m (List Double, a)
withPartialRandomness randomness k = let wpr = withPartialRandomness' randomness k
  -- let wpr = the (WriterT (List Double) m a) (withPartialRandomness' randomness k)
  in ?rh

||| Like `withPartialRandomness`, but use an arbitrary sampling monad.
runWith : {m : _} -> MonadSample m => List Double -> FreeSampler Identity a -> m (a, List Double)
runWith randomness = ?withPartialRandomness_hole randomness . hoist {n=m} (pure . runIdentity)
