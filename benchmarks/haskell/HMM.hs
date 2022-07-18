module HMM where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
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

--- Execute HMM

mhHMM :: Int -> Int -> IO ()
mhHMM n_samples n_steps = do
  ys <- undefined
  sampleIO $ prior $ mh n_samples (hmmPrior >>= hmm 0 (head ys))
  return ()
