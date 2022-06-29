module Control.Monad.Bayes.Interface

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import public Data.List
import public Data.Vect
import System.Random

import public Statistics.Distribution.Uniform
import public Statistics.Distribution.Normal
import public Statistics.Distribution.Beta
import public Statistics.Distribution.Gamma

import public Numeric.Log

%default total

-- draw : Double -> m Double
-- draw 

public export
interface Monad m => MonadSample m where
  ||| Must return in Uniform(0,1)
  random : m Double

  ||| Uniform(min, max)
  uniform : (min, max : Double) -> m Double
  uniform min max = map (Uniform.uniform_cdf_inv min max) random

  ||| N(mean, sd)
  normal : (mean, sd : Double) -> m Double
  normal m s      = map (Normal.normal_cdf_inv m s) random

  ||| gamma : (shape, scale : Double) -> m Double
  gamma : (a, b : Double) -> m Double
  gamma a b       = map (Gamma.gamma_cdf_inv a b) random

  ||| beta : (alpha, beta : Double) -> m Double
  beta : (a, b : Double) -> m Double
  beta a b        = map (Beta.beta_cdf_inv a b) random

  ||| Categorical(ps)
  categorical : Vect n Double -> m (Fin n)
  categorical ps = do
    r <- random
    let normalised_ps = map (/(sum ps)) ps 
    case findIndex (>= r) normalised_ps of
      Just i  => pure i
      Nothing => assert_total $ idris_crash "categorical: bad weights!"

  ||| Log-categorical(log-ps)
  logCategorical : Vect n (Log Double) -> m (Fin n)
  logCategorical logps = do
    let ps = map (exp . ln) logps
    categorical ps

  ||| uniformD : List a -> m a

  ||| geometric : Double -> m Int

  ||| poisson : Double -> m Int

  ||| dirichlet : Vect n Double -> m (Vect n Double)

  ||| Bern(p)
  bernoulli : (p : Double) -> m Bool
  bernoulli p = map (< p) random

  ||| B(n, p)
  binomial : (n : Nat) -> (p : Double) -> m Nat
  binomial n p = (pure . length . List.filter (== True)) !(sequence . replicate n $ bernoulli p)

  ||| DiscUniform(range); should return Nat from 0 to (range - 1)
  discreteUniform : (range : Nat) -> m Nat
  discreteUniform range = do
        r <- random
        pure $ cast (floor (cast range * r))

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
export
MonadSample m => MonadSample (MaybeT m) where
  random = lift random
  bernoulli = lift . bernoulli
export
MonadCond m => MonadCond (MaybeT m) where
  score = lift . score
export
MonadInfer m => MonadInfer (MaybeT m) where

-- ReaderT
export
MonadSample m => MonadSample (ReaderT r m) where
  random = lift random
  bernoulli = lift . bernoulli
export
MonadCond m => MonadCond (ReaderT r m) where
  score = lift . score
export
MonadInfer m => MonadInfer (ReaderT r m) where

-- WriterT
export
MonadSample m => MonadSample (WriterT w m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical
export
MonadCond m => MonadCond (WriterT w m) where
  score = lift . score
export
MonadInfer m => MonadInfer (WriterT w m) where

-- StateT
export
MonadSample m => MonadSample (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical
export
MonadCond m => MonadCond (StateT s m) where
  score = lift . score
export
MonadInfer m => MonadInfer (StateT s m) where

-- RWST
export
MonadSample m => MonadSample (RWST r w s m) where
  random = lift random
  bernoulli = lift . bernoulli
export
MonadCond m => MonadCond (RWST r w s m) where
  score = lift . score
export
MonadInfer m => MonadInfer (RWST r w s m) where
