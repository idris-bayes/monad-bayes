module Control.Monad.Bayes.Sequential

import Control.Monad.Trans
import Control.Monad.Bayes.Interface
 
total
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

  export
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

export
MonadCond m => MonadCond (Sequential m) where
  score w = lift (score w) >> suspend

export
MonadInfer m => MonadInfer (Sequential m) where

export
advance : Monad m => Sequential m a -> Sequential m a
advance (MkSeq m) = MkSeq (m >>= either ( pure . Left ) runSeq )

export
finish : Monad m => Sequential m a -> m a
finish (MkSeq m) = (m >>= either pure finish)

export
hoistFirst : (forall x. m x -> m x) -> Sequential m a -> Sequential m a
hoistFirst tau (MkSeq m) = MkSeq (tau m)

-- | Apply a function a given number of times.
export
composeCopies : Nat -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (List.replicate k f)

export
sis :
  Monad m =>
  -- | transformation
  (forall x. m x -> m x) ->
  -- | number of time steps
  Nat ->
  Sequential m a ->
  m a
sis f k = finish . composeCopies k (advance . hoistFirst f)
