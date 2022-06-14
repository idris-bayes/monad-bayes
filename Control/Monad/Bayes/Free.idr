module Control.Monad.Bayes.Free

import Control.Monad.Trans

import Control.Monad.Bayes.Interface
import Control.Monad.Free
import Control.Monad.Free.Church
--import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church

||| Random sampling functor
data SamF a = Random (Double -> a)

Functor SamF where
  map f (Random k) = Random (f . k)

||| Free monad transformer over random sampling.
||| Uses the Church-encoded version of the free monad for efficiency.
FreeSampler : (Type -> Type) -> Type -> Type
FreeSampler = FT SamF

--record FreeSampler m a where
  --constructor MkFreeSampler
  --runFreeSampler : FT SamF m a

-- TODO: check if this is needed, since we're just aliasing FT
--{m : _} -> Monad m => MonadFree SamF (FreeSampler m) where
  --wrap (Random x) = MkFT $ ?a

(Monad m, MonadFree SamF (FreeSampler m)) => MonadSample (FreeSampler m) where
  random = MkFT (?h (Random id))

||| Hoist 'FreeSampler' through a monad transform.
hoist : (Monad m, Monad n) => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoist = hoistFT

{-
||| Execute random sampling in the transformed monad.
interpret : MonadSample m => FreeSampler m a -> m a
interpret (MkFT m) = iterT f m
  where f : Monad m => SamF (FT SamF m a) -> m a
        f (Random k) = random >>= k
-}
