module Control.Monad.Bayes.Interface

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import public Data.List
import Data.Vect
import System.Random

--import Control.Monad.Trans.Identity
import public Statistics.Distribution.Uniform
import public Statistics.Distribution.Normal
--import Statistics.Distribution.Binomial

import Numeric.Log

%default total
-- TODO: implement more distributions
public export
interface Monad m => MonadSample m where
  ||| Must return in Uniform(0,1)
  random : m Double

  ||| Bern(p)
  bernoulli : (p : Double) -> m Bool
  bernoulli p = map (< p) random

  ||| Uniform(min, max)
  uniform : (min, max : Double) -> m Double
  uniform min max = map (Uniform.uniform min max) random

  ||| N(mean, sd)
  normal : (mean, sd : Double) -> m Double
  normal m s = do
         r1 <- random
         r2 <- random
         pure $ Normal.normal m s r1 r2

  ||| B(n, p)
  binomial : (n : Nat) -> (p : Double) -> m Nat
  binomial n p = (pure . length . List.filter (== True)) !(sequence . replicate n $ bernoulli p)

  ||| DiscUniform(range); should return Nat from 0 to (range - 1)
  discreteUniform : (range : Nat) -> m Nat
  discreteUniform range = do
        r <- random
        pure $ cast (floor (cast range * r))

  ||| Categorical(ps)
  categorical : Vect n Double -> m (Fin n)
  categorical ps = do
    r <- random
    let total_ps = sum ps 
        normalised_ps = map (/total_ps) ps 
    case findIndex (>= r) normalised_ps of
      Just i  => pure i
      Nothing => assert_total $ idris_crash "categorical: bad weights!"

  ||| Log-categorical(log-ps)
  logCategorical : Vect n (Log Double) -> m (Fin n)
  logCategorical logps = do
    let ps = map (exp . ln) logps
    categorical ps

public export
interface Monad m => MonadCond m where
  ||| Record a likelihood
  score : Log Double -> m ()  

export
condition : MonadCond m => Bool -> m ()
condition b = score $ if b then 1 else 0

public export
interface (MonadSample m, MonadCond m) => MonadInfer m where

-- MaybeT
MonadSample m => MonadSample (MaybeT m) where
  random = lift random
--  bernoulli = lift . bernoulli
MonadCond m => MonadCond (MaybeT m) where
  score = lift . score
MonadInfer m => MonadInfer (MaybeT m) where

-- ReaderT
MonadSample m => MonadSample (ReaderT r m) where
  random = lift random
  bernoulli = lift . bernoulli
MonadCond m => MonadCond (ReaderT r m) where
  score = lift . score
MonadInfer m => MonadInfer (ReaderT r m) where

-- WriterT
MonadSample m => MonadSample (WriterT w m) where
  random = lift random
  bernoulli = lift . bernoulli
MonadCond m => MonadCond (WriterT w m) where
  score = lift . score
MonadInfer m => MonadInfer (WriterT w m) where

-- StateT
MonadSample m => MonadSample (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli
MonadCond m => MonadCond (StateT s m) where
  score = lift . score
MonadInfer m => MonadInfer (StateT s m) where

-- RWST
MonadSample m => MonadSample (RWST r w s m) where
  random = lift random
  bernoulli = lift . bernoulli
MonadCond m => MonadCond (RWST r w s m) where
  score = lift . score
MonadInfer m => MonadInfer (RWST r w s m) where
