module Control.Monad.Bayes.Sequential

public export
data Lift : (m : Type -> Type) -> Type -> Type where
  MkLift : m a -> Lift m a

-- | 
public export
data Seq : (m : Type -> Type) -> (a : Type) -> Type where
  L : a -> Seq m a
  R : (op : Lift m x) -> (k : x -> Seq m a) -> Seq m a

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
implementation Monad (Seq es) where
  R op k >>= f = R op ( assert_total (>>= f) . k)
  L x   >>= f  = f x

-- runSeq : S