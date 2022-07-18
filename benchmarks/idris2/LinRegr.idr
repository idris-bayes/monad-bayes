module LinRegr

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
import Numeric.Log

||| Params
record LinRegrParams where
  constructor MkLinRegrParams
  m : Double   -- mean
  c : Double      -- intercept
  s : Double      -- standard deviation

||| Prior
linRegrPrior : MonadSample m => m LinRegrParams
linRegrPrior = do
  m <- normal 0 3
  c <- normal 0 5
  s <- uniform 1 3
  pure (MkLinRegrParams m c s)

||| Model
linRegr : MonadInfer m => List (Double, Double) -> LinRegrParams -> m LinRegrParams
linRegr xys (MkLinRegrParams m0 c0 s0) = do
  _ <- sequence (map (\(x, y_obs) => let logprob : Log Double = toLogDomain (gsl_normal_pdf (m0 * x + c0) s0 y_obs )
                                     in  score logprob) xys)
  pure (MkLinRegrParams m0 c0 s0)

||| Data
mkLinRegrData : Nat -> IO (List (Double, Double))
mkLinRegrData n_datapoints = sampleIO $ do
  MkLinRegrParams m c s <- linRegrPrior
  let xs = map cast [0 ..  n_datapoints]
  ys <- sequence (map (\x => normal (m * x + c) s) xs)
  pure (zip xs ys)

||| MH
mhLinRegr : (n_mhsteps : Nat) -> Nat -> IO ()
mhLinRegr n_mhsteps n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  _       <- sampleIO $ prior $ mh n_mhsteps (linRegrPrior >>= linRegr dataset)
  pure ()

||| SMC
smcLinRegr : (n_particles : Nat) -> Nat -> IO ()
smcLinRegr n_particles n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  _       <- sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles (linRegrPrior >>= linRegr dataset)
  pure ()

||| RMSMC
rmsmcLinRegr : (n_particles : Nat) -> (n_mhsteps : Nat) ->  Nat -> IO ()
rmsmcLinRegr n_particles n_mhsteps  n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  _       <- sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps (linRegrPrior >>= linRegr dataset)
  pure ()
