{- | Latent Dirichlet Allocation -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module LDA where

import Control.Monad.Bayes.Class
    ( MonadCond(score),
      MonadInfer,
      MonadSample(categorical, dirichlet) )
import Control.Monad.Bayes.Traced.Basic ( mh )
import Control.Monad.Bayes.Sampler ( sampleIO, sampleIOfixed )
import Control.Monad.Bayes.Weighted ( prior )
import Control.Monad.Bayes.Population ( runPopulation )
import Control.Monad.Bayes.Inference.SMC ( smcSystematic )
import Control.Monad.Bayes.Inference.RMSMC ( rmsmc )
import Criterion (benchmark)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import Numeric.Log ( Log(Exp) )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import Data.Vector (Vector, fromList, toList)
import Data.List ( elemIndex )

-- | Params
data LDAParams = LDAParams {
    θ :: [Double],   -- probabilities of each topic in a document
    φ :: [[Double]]  -- probabilities of each word in a topic in a document
   } deriving Show

-- | Prior
topicPrior :: (MonadSample m) => Int -> [String] -> m LDAParams
topicPrior n_topics vocab = do
  -- Distribution over topics for a given document
  doc_topic_ps <- toList <$> dirichlet (fromList $ replicate n_topics 1)
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics (toList <$> dirichlet (fromList $ replicate (length vocab) 1))
  return (LDAParams doc_topic_ps topic_word_ps)

-- | Model
topicModel :: MonadInfer m => [String] -> [String] -> LDAParams -> m LDAParams
topicModel vocab words (LDAParams doc_topic_ps topic_word_ps) = do
  let scoreWords [] = return words
      scoreWords (w:ws) = do
        z <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! z
            w_p     = word_ps !! fromJust (elemIndex w vocab)
        score (Exp w_p)
        scoreWords ws
  scoreWords words
  return (LDAParams doc_topic_ps topic_word_ps)

-- | Data
fixed_vocab :: [String]
fixed_vocab = ["DNA", "evolution", "parsing", "phonology"]

fixed_n_topics :: Int
fixed_n_topics = 2

document :: [String]
document = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA", "DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

mkTopicData :: Int -> [String]
mkTopicData = flip take document

-- | MH
mhLDA :: Int -> Int -> IO ()
mhLDA n_mhsteps n_words = do
  sampleIOfixed $ prior $ mh n_mhsteps
    (topicPrior 2 fixed_vocab >>= topicModel fixed_vocab (mkTopicData n_words))
  return ()

-- | SMC
smcLinRegr :: Int -> Int -> IO ()
smcLinRegr n_particles n_datapoints = do
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles
    (topicPrior fixed_n_topics fixed_vocab >>= topicModel fixed_vocab (mkTopicData n_datapoints))
  return ()

-- | RMSMC
rmsmcLinRegr :: Int -> Int -> Int -> IO ()
rmsmcLinRegr n_particles n_mhsteps n_datapoints = do
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps
    (topicPrior fixed_n_topics fixed_vocab >>= topicModel fixed_vocab (mkTopicData n_datapoints))
  return ()
