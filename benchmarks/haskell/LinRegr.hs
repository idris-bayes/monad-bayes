{- | Lin Regression -}

module LinRegr where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Criterion (benchmark)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import Data.Vector (Vector, fromList, toList)

-- | Params
data LinRegrParams = LinRegrParams {
    m :: Double
  , c :: Double
  , σ :: Double
} deriving Show

fromLinRegrParams :: LinRegrParams -> (Double, Double, Double)
fromLinRegrParams (LinRegrParams m c σ) = (m, c, σ)

-- | Prior
linRegrPrior :: MonadSample m => m LinRegrParams
linRegrPrior = do
  m <- normal 0 3
  c <- normal 0 5
  σ <- uniform 1 3
  return (LinRegrParams m c σ)

-- | Model
linRegr :: MonadInfer m => [(Double, Double)] -> LinRegrParams -> m LinRegrParams
linRegr xys (LinRegrParams m c σ) = do
  mapM_ (\(x, y_obs) -> score (normalPdf (m * x + c) σ y_obs)) xys
  return (LinRegrParams m c σ)

-- | Data
mkLinRegrData :: Int -> IO [(Double, Double)]
mkLinRegrData n_datapoints = sampleIO $ do
  LinRegrParams m c σ <- linRegrPrior

  let xs = [0 .. (fromIntegral n_datapoints)]
  ys <- mapM (\x -> normal (m * x + c) σ) xs

  return (zip xs ys)

-- | MH
mhLinRegr :: Int -> Int -> IO ()
mhLinRegr n_mhsteps n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  sampleIO $ prior $ mh n_mhsteps (linRegrPrior >>= linRegr dataset)
  return ()

-- | SMC
smcLinRegr :: Int -> Int -> IO ()
smcLinRegr n_particles n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles (linRegrPrior >>= linRegr dataset)
  return ()

-- | RMSMC
rmsmcLinRegr :: Int -> Int -> Int -> IO ()
rmsmcLinRegr n_particles n_mhsteps n_datapoints = do
  dataset <- mkLinRegrData n_datapoints
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps (linRegrPrior >>= linRegr dataset)
  return ()
