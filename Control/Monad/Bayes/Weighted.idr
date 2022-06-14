||| Weighted is an instance of MonadCond. Apply a MonadSample transformer to
||| obtain a MonadInfer that can execute probabilistic models.

module Control.Monad.Bayes.Weighted

import Control.Monad.State

import Control.Monad.Bayes.Class

Weighted : (m : Type -> Type) -> (a : Type) -> Type
Weighted m a = StateT Double m a  -- TODO: replace Double with Log

Functor f => Functor (Weighted m) where
  map f (ST g) = ?h

x : ST

--Monad m => MonadCond (Weighted m) where
  --score w = Weighted (modify (* w))
