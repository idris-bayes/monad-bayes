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
record Params where
  constructor MkParams
  mu : Double     -- mean
  c : Double      -- intercept
  s : Double      -- standard deviation

Show Params where
  show (MkParams mv cv sv) = "(m : " ++ show mv ++ ", c : " ++ show cv ++ ", std : " ++ show sv ++ ")"

||| Prior
linRegrPrior : MonadSample m => m Params
linRegrPrior = do
  mu <- normal 0 3
  c <- normal 0 5
  s <- uniform 1 3
  pure (MkParams mu c s)

||| Model
linRegr : MonadInfer m => List (Double, Double) -> Params -> m Params
linRegr xys (MkParams m0 c0 s0) = do
  _ <- sequence (map (\(x, y_obs) => let logprob : Log Double = toLogDomain (gsl_normal_pdf (m0 * x + c0) s0 y_obs )
                                     in  score logprob) xys)
  pure (MkParams m0 c0 s0)

||| Data
mkLinRegrData : Nat -> IO (List (Double, Double))
mkLinRegrData n_datapoints = sampleIO $ do
  MkParams mu c s <- linRegrPrior
  let xs = map cast [0 ..  n_datapoints]
  ys <- sequence (map (\x => normal (mu * x + c) s) xs)
  pure (zip xs ys)

||| MH
export
mhLinRegr
  :  (n_mhsteps : Nat)
  -> Nat
  -> IO ()
mhLinRegr n_mhsteps n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  xs      <- sampleIO $ prior $ mh n_mhsteps (linRegrPrior >>= linRegr dataset)
  -- print xs
  pure ()

||| SMC
export
smcLinRegr
  :  (n_particles : Nat)
  -> Nat
  -> IO ()
smcLinRegr n_particles n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  xs       <- sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles (linRegrPrior >>= linRegr dataset)
  -- print xs
  pure ()

||| RMSMC
export
rmsmcLinRegr
  :  (n_particles : Nat)
  -> (n_mhsteps : Nat)
  -> Nat
  -> IO ()
rmsmcLinRegr n_particles n_mhsteps  n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  xs      <- sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps (linRegrPrior >>= linRegr dataset)
  -- print xs
  pure ()

||| PMMH
export
pmmhLinRegr
  :  (n_mhsteps : Nat)
  -> (n_timesteps : Nat) -> (n_particles : Nat) -> Nat
  -> IO (Vect (S n_mhsteps) (List (Log Double, Params)))
pmmhLinRegr n_mhsteps n_timesteps n_particles n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  param_trace <- sampleIO $ prior $
                  pmmh n_mhsteps n_timesteps n_particles linRegrPrior (linRegr dataset)
  print param_trace >> pure param_trace

||| SMC2
export
smc2LinRegr
  :  (n_timesteps : Nat)
  -> (n_inner_particles : Nat)
  -> (n_outer_particles : Nat)
  -> (n_mhsteps : Nat)
  -> Nat
  -> IO (List (Log Double, (List (Log Double, Params))))
smc2LinRegr n_timesteps n_inner_particles  n_outer_particles n_mhsteps n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  param_trace <- sampleIO $ runPopulation $
                  smc2 n_timesteps n_inner_particles n_outer_particles n_mhsteps linRegrPrior (linRegr dataset)
  print param_trace >> pure param_trace

{-
pack --with-ipkg examples.ipkg repl LinRegr.idr
-}

