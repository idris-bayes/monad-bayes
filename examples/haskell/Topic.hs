{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{- | Latent Dirichlet Allocation -}

module Topic where

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
data Params = Params {
    θ :: [Double],   -- probabilities of each topic in a document
    φ :: [[Double]]  -- probabilities of each word in a topic in a document
   } deriving Show

-- | Prior
topicPrior :: (MonadSample m) => Int -> [String] -> m Params
topicPrior n_topics vocab = do
  -- Distribution over topics for a given document
  doc_topic_ps <- toList <$> dirichlet (fromList $ replicate n_topics 1)
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics (toList <$> dirichlet (fromList $ replicate (length vocab) 1))
  return (Params doc_topic_ps topic_word_ps)

-- | Model
topicModel :: MonadInfer m => [String] -> [String] -> Params -> m Params
topicModel vocab words (Params doc_topic_ps topic_word_ps) = do
  let scoreWords [] = return words
      scoreWords (w:ws) = do
        topic_idx <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! topic_idx
            w_p     = word_ps !! fromJust (elemIndex w vocab)
        score (Exp w_p)
        scoreWords ws
  scoreWords words
  return (Params doc_topic_ps topic_word_ps)

-- | Data
fixed_vocab :: [String]
fixed_vocab = ["DNA", "evolution", "parsing", "phonology"]

fixed_n_topics :: Int
fixed_n_topics = 2

mkTopicData :: Int -> IO [String]
mkTopicData n_words = sampleIO $ do
  (Params doc_topic_ps topic_word_ps) <- topicPrior fixed_n_topics fixed_vocab
  let genWord = do
        topic_idx <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! topic_idx
        word_idx <- categorical (fromList word_ps)
        return (fixed_vocab !! word_idx)
  replicateM n_words genWord

-- | MH
mhTopic :: Int -> Int -> IO ()
mhTopic n_mhsteps n_words = do
  dataset <- mkTopicData n_words
  sampleIOfixed $ prior $ mh n_mhsteps
    (topicPrior 2 fixed_vocab >>= topicModel fixed_vocab dataset)
  return ()

-- | SMC
smcTopic :: Int -> Int -> IO ()
smcTopic n_particles n_words = do
  dataset <- mkTopicData n_words
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ smcSystematic n_timesteps n_particles
    (topicPrior fixed_n_topics fixed_vocab >>= topicModel fixed_vocab dataset)
  return ()

-- | RMSMC
rmsmcTopic :: Int -> Int -> Int -> IO ()
rmsmcTopic n_particles n_mhsteps n_words = do
  dataset <- mkTopicData n_words
  let n_timesteps = n_particles
  sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps
    (topicPrior fixed_n_topics fixed_vocab >>= topicModel fixed_vocab dataset)
  return ()
