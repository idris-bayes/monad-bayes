module Control.Monad.Bayes.Sequential

import Control.Monad.Trans
import Control.Monad.Bayes.Interface

||| Represents a computation that can be suspended at certain points.
-- The intermediate monadic effects can be extracted, which is particularly
-- useful for implementation of Sequential Monte Carlo related methods.
-- All the probabilistic effects are lifted from the transformed monad, but
-- also `suspend` is inserted after each `score`.
export
data Sequential : (m : Type -> Type) -> (a : Type) -> Type where
  MkSeq : Inf (m (Either a (Sequential m a))) -> Sequential m a

export
runSeq : Sequential m a -> m (Either a (Sequential m a))
runSeq (MkSeq m) = m

mutual
  export
  Monad m => Functor (Sequential m) where
    map f (MkSeq mx) = MkSeq (do
      x <- mx
      case x of Left l  => pure (Left $ f l)
                Right r => pure (Right $ map f r))
  export
  Monad m => Applicative (Sequential m) where
    pure x = MkSeq (pure (Left x))
    mf <*> mv = do
      f' <- mf
      v' <- mv
      pure $ f' v'

  export covering
  Monad m => Monad (Sequential m) where
    (>>=) (MkSeq mx) f = MkSeq $ do
      x <- mx
      case x of
        Left l    => runSeq (f l)
        Right seq => pure (Right ((assert_total (>>=)) seq  f))

  export
  MonadTrans Sequential where
    lift mx = MkSeq (map Left mx)

export
suspend : Monad m => Sequential m ()
suspend = MkSeq (pure (Right (pure ())))

export
MonadSample m => MonadSample (Sequential m) where
  random = lift random

||| Execution is suspended after each 'score'.
export
MonadCond m => MonadCond (Sequential m) where
  score w = lift (score w) >> suspend

export
MonadInfer m => MonadInfer (Sequential m) where

export
||| Execute to the next suspension point. If the computation is finished, do nothing.
advance : Monad m => Sequential m a -> Sequential m a
advance (MkSeq m) = MkSeq (m >>= either ( pure . Left ) runSeq )

export
||| Remove the remaining suspension points.
finish : Monad m => Sequential m a -> m a
finish (MkSeq m) = (m >>= either pure finish)

export
||| Transform the inner monad. This operation only applies to computation up to the first suspension.
hoistFirst : (forall x. m x -> m x) -> Sequential m a -> Sequential m a
hoistFirst tau (MkSeq m) = MkSeq (tau m)

||| Apply a function a given number of times.
export
composeCopies : Nat -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (List.replicate k f)

||| Sequential importance sampling. Applies a given transformation after each time step.
export
sis :
  Monad m =>
  -- | transformation
  (forall x. m x -> m x) ->
  -- | number of time steps
  (n_timesteps : Nat) ->
  Sequential m a ->
  m a
sis f k = finish . composeCopies k (advance . hoistFirst f)
