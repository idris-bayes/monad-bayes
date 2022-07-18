module HMM

import Data.Maybe
import Data.List
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.PMMH
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC2
import Statistics.Distribution.Normal
import Statistics.Distribution.Binomial
import Numeric.Log

||| Util
boolToNat : Bool -> Nat
boolToNat True  = 1
boolToNat False = 0

||| Params
record Params where
  constructor MkParams
  transition_p : Double
  observation_p : Double

||| Prior
hmmPrior : MonadSample m => m Params
hmmPrior = do
  trans_p <- uniform 0 1
  obs_p   <- uniform 0 1
  pure (MkParams trans_p obs_p)

||| Model
obsModel : MonadInfer m => Double -> Nat -> Nat -> m Nat
obsModel observation_p x y_obs = do
  score (toLogDomain $ gsl_binomial_pdf x observation_p y_obs)
  pure y_obs

transModel : MonadSample m => Double -> Nat -> m Nat
transModel transition_p x = do
  dX <- bernoulli transition_p
  pure (x + boolToNat dX)

hmmNode : MonadInfer m => Nat -> Nat -> Params ->  m Nat
hmmNode x y_obs (MkParams transition_p observation_p) = do
  x' <- transModel transition_p x
  y' <- obsModel observation_p x' y_obs
  pure x'

hmm : MonadInfer m => Nat -> List Nat -> Params -> m Params
hmm x [] params = pure params
hmm x (y::ys) params = do
  x' <- hmmNode x y params
  hmm x' ys params

||| Data
fixed_init_state : Nat
fixed_init_state = 0

mkHMMData : Nat -> IO (List Nat)
mkHMMData n_nodes = sampleIO $ do
  MkParams trans_p obs_p <- hmmPrior
  let genData : Nat -> Nat -> IO (List Nat)
      genData i x =
        if (i >= n_nodes)
          then (pure [])
          else (do  x' <- transModel trans_p x
                    y' <- binomial x' obs_p
                    ys <- genData (i + 1) x'
                    pure (y'::ys))
  genData 0 fixed_init_state

||| MH
mhHMM : Nat -> Nat -> IO ()
mhHMM n_mhsteps n_nodes = do
  dataset <- mkHMMData n_nodes
  _       <- sampleIO $ prior $ mh n_mhsteps
                (hmmPrior >>= hmm fixed_init_state dataset)
  pure ()

||| SMC
smcHMM : Nat -> Nat -> IO ()
smcHMM n_particles n_nodes = do
  dataset <- mkHMMData n_nodes
  let n_timesteps = n_particles
  _       <- sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles
                (hmmPrior >>= hmm fixed_init_state dataset)
  pure ()

||| RMSMC
rmsmcHMM : Nat -> Nat -> Nat -> IO ()
rmsmcHMM n_particles n_mhsteps n_nodes = do
  dataset <- mkHMMData n_nodes
  let n_timesteps = n_particles
  _       <- sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps
                (hmmPrior >>= hmm fixed_init_state dataset)
  pure ()
