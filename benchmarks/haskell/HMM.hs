{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module HMM where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Population ( runPopulation )
import Control.Monad.Bayes.Inference.SMC ( smcSystematic )
import Control.Monad.Bayes.Inference.RMSMC ( rmsmc )
import Criterion (benchmark)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import           Numeric.Log
import Data.Vector (Vector, fromList, toList)

-- | Util
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

binomial :: MonadSample m => Int -> Double -> m Int
binomial n p = discrete $ SB.binomial n p

binomialPdf :: Int -> Double -> Int -> Log Double
binomialPdf n p y = Exp $ logProbability (SB.binomial n p) y

-- | Params
data Params = Params {
    transition_p  :: {-# UNPACK #-} !Double,
    observation_p :: {-# UNPACK #-} !Double
} deriving Show

-- | Prior
hmmPrior :: MonadSample m => m Params
hmmPrior = do
  trans_p <- uniform 0 1
  obs_p   <- uniform 0 1
  return (Params trans_p obs_p)

-- | Model
obsModel :: MonadInfer m => Double -> Int -> Int -> m Int
obsModel observation_p x y_obs = do
  score (binomialPdf x observation_p y_obs)
  return y_obs

transModel :: MonadSample m => Double -> Int -> m Int
transModel transition_p x = do
  dX <- bernoulli transition_p
  return (x + boolToInt dX)

hmmNode :: MonadInfer m => Int -> Int -> Params ->  m Int
hmmNode x y_obs (Params transition_p observation_p) = do
  x' <- transModel transition_p x
  y' <- obsModel observation_p x' y_obs
  return x'

hmm :: MonadInfer m => Int -> [Int] -> Params -> m Params
hmm  x [] params = return params
hmm  x (y:ys) params = do
  x' <- hmmNode x y params
  hmm x' ys params

-- | Data
fixed_init_state :: Int
fixed_init_state = 0

mkHMMData :: Int -> IO [Int]
mkHMMData n_nodes = sampleIO $ do
  Params trans_p obs_p <- hmmPrior
  let genData i x
        | i >= n_nodes = return []
        | otherwise = do
            x' <- transModel trans_p x
            y' <- binomial x' obs_p
            ys <- genData (i + 1) x'
            return (y':ys)
  genData 0 fixed_init_state

-- | MH
mhHMM :: Int -> Int -> IO ()
mhHMM n_mhsteps n_nodes = do
  dataset <- mkHMMData n_nodes
  sampleIO $ prior $ mh n_mhsteps (hmmPrior >>= hmm fixed_init_state dataset)
  return ()

-- | SMC
smcHMM :: Int -> Int -> IO ()
smcHMM n_particles n_nodes = do
  dataset <- mkHMMData n_nodes
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles
    (hmmPrior >>= hmm fixed_init_state dataset)
  return ()

-- | RMSMC
rmsmcHMM :: Int -> Int -> Int -> IO ()
rmsmcHMM n_particles n_mhsteps n_nodes = do
  dataset <- mkHMMData n_nodes
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps
    (hmmPrior >>= hmm fixed_init_state dataset)
  return ()
