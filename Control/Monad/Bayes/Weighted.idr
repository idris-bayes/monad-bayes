||| Weighted is an instance of MonadCond. Apply a MonadSample transformer to
||| obtain a MonadInfer that can execute probabilistic models.

module Control.Monad.Bayes.Weighted

import public Control.Monad.State

import Control.Monad.Bayes.Interface

import public Numeric.Log

||| Execute the program using the prior distribution, while accumulating likelihood.
public export
record Weighted (m : Type -> Type) (a : Type) where
  constructor MkWeighted
  runWeighted' : StateT (Log Double) m a 

||| Obtain an explicit value of the likelihood for a given value
public export
runWeighted : Weighted m a -> m (Log Double, a)
runWeighted (MkWeighted m) = runStateT 1 m 

export
MonadTrans Weighted where
  lift = MkWeighted . lift

export
Functor m => Functor (Weighted m) where
  map f (MkWeighted s) =  (MkWeighted $ map f s) 

export
Monad m => Applicative (Weighted m) where
  pure x = MkWeighted (pure x)
  (MkWeighted mf) <*> (MkWeighted ma) = MkWeighted (mf <*> ma) 
  
export
Monad m => Monad (Weighted m) where
  (MkWeighted mx) >>= k = MkWeighted (mx >>= (runWeighted' . k))

export
MonadSample m => MonadSample (Weighted m) where
  random = lift random

export
Monad m => MonadCond (Weighted m) where
  score w = MkWeighted (modify (* w))

export
MonadSample m => MonadInfer (Weighted m) where

||| Compute the sample and discard the weight.
||| This operation introduces bias.
export
prior : Functor m => Weighted m a -> m a
prior = map snd . runWeighted

||| Compute the weight and discard the sample.
export
extractWeight : Functor m => Weighted m a -> m (Log Double)
extractWeight = map fst . runWeighted

||| Embed a random variable with explicitly given likelihood
export
withWeight : Monad m => m (Log Double, a) -> Weighted m a
withWeight m = MkWeighted $ do
  (w, x) <- lift m
  modify (* w)
  pure x

||| Combine weights from two different levels.
export
flatten : Monad m => Weighted (Weighted m) a -> Weighted m a
flatten m = withWeight $ (\(p, (q, x)) => (p * q, x)) <$> runWeighted (runWeighted m)

||| Use the weight as a factor in the transformed monad.
export
applyWeight : MonadCond m => Weighted m a -> m a
applyWeight m = do
  (w, x) <- runWeighted m
  score w
  pure x

||| Apply a transformation to the transformed monad.
export
hoist : (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoist phi (MkWeighted w) =  MkWeighted $ mapStateT phi w
