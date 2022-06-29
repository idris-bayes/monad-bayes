module Control.Monad.Bayes.Seq

import Control.Monad.Trans
import Control.Monad.Bayes.Interface

total
data Seq : (m : Type -> Type) -> (a : Type) -> Type where 
  MkSeq : Inf (m (Either a (Seq m a))) -> Seq m a

runSeq : Seq m a -> m (Either a (Seq m a))
runSeq (MkSeq m) = m

mutual 
  Monad m => Functor (Seq m) where
    map f (MkSeq mx) = MkSeq (do
      x <- mx 
      case x of Left l  => pure (Left $ f l)
                Right r => pure (Right $ map f r))

  Monad m => Applicative (Seq m) where
    pure x = MkSeq (pure (Left x))
    mf <*> mv = do
      f' <- mf
      v' <- mv
      pure $ f' v'

  Monad m => Monad (Seq m) where
    (>>=) (MkSeq mx) f = MkSeq $ do
      x <- mx
      case x of
        Left l    => runSeq (f l) 
        Right seq => pure (Right ((assert_total (>>=)) seq  f))

  MonadTrans Seq where
    lift mx = MkSeq (map Left mx)

suspend : Monad m => Seq m ()
suspend = MkSeq (pure (Right (pure ())))

MonadSample m => MonadSample (Seq m) where
  random = lift random

MonadCond m => MonadCond (Seq m) where
  score w = lift (score w) >> suspend

advance : Monad m => Seq m a -> Seq m a
advance (MkSeq m) = MkSeq (m >>= either ( pure . Left ) runSeq )

finish : Monad m => Seq m a -> m a
finish (MkSeq m) = (m >>= either pure finish)

hoistFirst : (forall x. m x -> m x) -> Seq m a -> Seq m a
hoistFirst tau (MkSeq m) = MkSeq (tau m)

-- | Apply a function a given number of times.
composeCopies : Nat -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (List.replicate k f)

sis :
  Monad m =>
  -- | transformation
  (forall x. m x -> m x) ->
  -- | number of time steps
  Nat ->
  Seq m a ->
  m a
sis f k = finish . composeCopies k (advance . hoistFirst f)
