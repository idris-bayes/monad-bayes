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

import public Statistics.Distribution

import public Numeric.Log

%default total

public export
interface Monad m => MonadSample m where
  ||| Draws a random value from Uniform(0,1)
  random : m Double

  ||| Uniform(min, max)
  uniform : (min, max : Double) -> m Double
  uniform min max = map (gsl_uniform_cdf_inv min max) random

  ||| Normal(mean, sd)
  normal : (mean, sd : Double) -> m Double
  normal m s      = map (gsl_normal_cdf_inv m s) random

  ||| Gamma(shape, scale) -> m Double
  gamma : (a, b : Double) -> m Double
  gamma a b       = map (gsl_gamma_cdf_inv a b) random

  ||| Beta(alpha, beta) -> m Double
  beta : (a, b : Double) -> m Double
  beta a b        = map (gsl_beta_cdf_inv a b) random

  ||| Bernoulli(prob)
  bernoulli : (p : Double) -> m Bool
  bernoulli p     = map (< p) random

  ||| Binomial(num trials, prob of each trial)
  binomial : (n : Nat) -> (p : Double) -> m Nat
  binomial n p = (pure . length . List.filter (== True)) !(sequence . replicate n $ bernoulli p)

  ||| Categorical(probs)
  categorical : {n : Nat} -> Vect n Double -> m (Fin n)
  categorical ps = do
    r <- random
    let normalised_ps = map (/(sum ps)) ps 

        cmf : Double -> Nat -> List Double -> Maybe (Fin n)
        cmf acc idx (x :: xs) = let acc' = acc + x 
                                in  if acc' > r then natToFin idx n else cmf acc' (S idx) xs
        cmf acc idx []        = Nothing

    case cmf 0 0 (toList normalised_ps) of
      Just i  => pure i
      Nothing => assert_total $ idris_crash $ "categorical: bad weights!" ++ show ps

  ||| Log-categorical(log-probs)
  logCategorical : {n : _} -> Vect n (Log Double) -> m (Fin n)
  logCategorical logps = categorical (map (exp . ln) logps)

  ||| Uniform-Discrete(values)
  uniformD : {n : Nat} -> Vect (S n) a -> m a
  uniformD xs = do
    idx <- categorical $ replicate (S n) (1 / cast n)
    pure (index idx xs)

  ||| Dirichlet(concentrations)
  dirichlet : Vect n Double -> m (Vect n Double)
  dirichlet as = do
    xs <- sequence $ map (`gamma` 1) as
    let s  = sum xs
        ys = map (/ s) xs
    pure ys

  ||| DiscUniform(range); should return Nat from 0 to (range - 1)
  discreteUniform : (range : Nat) -> m Nat
  discreteUniform range = do
        r <- random
        pure $ cast (floor (cast range * r))

  ||| Draw from a discrete distribution using the probability mass function and a sequence of draws from Bernoulli.
  fromPMF : (pmf : Nat -> Double) -> m Nat
  fromPMF pmf = f 0 1
    where
      f : Nat -> Double -> m Nat
      f n marginal with (marginal < 0)
       _ | False = do
                let prob = pmf n
                b <- bernoulli (prob / marginal)
                if b then pure n else assert_total f (n + 1) (marginal - prob)
       _ | True = assert_total $ idris_crash "fromPMF: total PMF above 1"

  ||| Geometric(prob)
  geometric : (p : Double) -> m Nat
  geometric p = fromPMF (gsl_geometric_pdf p)

  ||| Poisson(λ)
  poisson : (p : Double) -> m Nat
  poisson p = fromPMF (gsl_poisson_pdf p)

public export
interface Monad m => MonadCond m where
  ||| Record a likelihood. Note: when calling `score (Exp p)`, p must already be in the log-domain.
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
