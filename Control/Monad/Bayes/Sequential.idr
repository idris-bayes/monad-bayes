module Control.Monad.Bayes.Sequential


-- data Seq : (m : Type -> Type) -> (a : Type) -> Type where 
--   MkSeq : m (Either a (Seq m a)) -> Seq m a

-- runSeq : Seq m a -> m (Either a (Seq m a))
-- runSeq (MkSeq m) = assert_total m

-- Functor m => Functor (Seq m) where
--   map h (MkSeq m) = MkSeq $ map (\case (Left  l) => Left (h l) 
--                                        (Right r) => Right (map h r)) m

-- liftA2 : Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f fa = (<*>) (map f fa)

-- Applicative m => Applicative (Seq m) where
--   pure x                    = MkSeq (pure (Left x))
--   (MkSeq mf) <*> (MkSeq ma) = ?todo

-- Monad m => Monad (Seq m) where
--   (>>=) (MkSeq mx) f = assert_total $ MkSeq $ do
--     x <- mx
--     case x of
--       Left l    => runSeq (f l) 
--       Right seq => pure (Right (seq >>= f))

--   -- join m = ?t