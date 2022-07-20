module Topic

import Data.Maybe
import Data.Vect
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

||| Params
record Params n_topics vocab_size where
  constructor MkParams
  theta : Vect n_topics Double                    -- probabilities of each topic in a document
  phi   : Vect n_topics (Vect vocab_size Double)  -- probabilities of each word in a topic in a document

Show (Params n m) where
  show (MkParams theta phi) = "(doc_topic_ps : " ++ show theta ++ ", topic_word_ps : " ++ show phi ++ ")"

fixed_vocab : Vect 4 String
fixed_vocab = ["DNA", "evolution", "parsing", "phonology"]

fixed_n_topics : Nat
fixed_n_topics = 2

||| Prior
topicPrior : (MonadSample m)
  => (n_topics : Nat)
  -> (vocab : Vect vocab_size String)
  -> m (Params n_topics (length vocab))
topicPrior n_topics vocab = do
  -- Distribution over topics for a given document
  doc_topic_ps <- dirichlet (Vect.replicate n_topics 1)
  -- Generate distribution over words for each topic
  topic_word_ps <- sequence (Vect.replicate n_topics (dirichlet (Vect.replicate (length vocab) 1)))
  pure (MkParams doc_topic_ps topic_word_ps)

||| Data
mkTopicData : Nat -> IO (List String)
mkTopicData n_words = sampleIO $ do
  (MkParams doc_topic_ps topic_word_ps) <- topicPrior fixed_n_topics fixed_vocab
  let genWord = do
        topic_idx <- categorical doc_topic_ps
        let word_ps = index topic_idx topic_word_ps
        word_idx <- categorical word_ps
        pure (index word_idx fixed_vocab)
  sequence (replicate n_words genWord)

||| Model
topicModel : (vocab_size_sub1 : Nat)
  => MonadInfer m
  => (vocab : Vect (S (vocab_size_sub1)) String)
  -> (doc   : List String)
  -> Params 2 (S (vocab_size_sub1))
  -> m (Params 2 (S (vocab_size_sub1)))
topicModel vocab words (MkParams doc_topic_ps topic_word_ps) = do
  let scoreWords : List String -> m ()
      scoreWords [] = pure ()
      scoreWords (w::ws) = do
        topic_idx <- categorical doc_topic_ps
        let word_ps : Vect (S (vocab_size_sub1)) Double
            word_ps = index topic_idx topic_word_ps
            word_idx : Fin (S (vocab_size_sub1))
            word_idx = case findIndex (==w) vocab of
                Just w_idx => w_idx
                Nothing    => restrict vocab_size_sub1 0
            w_p : Double
            w_p = index word_idx word_ps
        score (toLogDomain w_p)
        scoreWords ws
  scoreWords words
  pure (MkParams doc_topic_ps topic_word_ps)

||| MH
export
mhTopic : Nat -> Nat -> IO ()
mhTopic n_mhsteps n_words = do
  dataset <- mkTopicData n_words
  xs <- sampleIO $ prior $ mh n_mhsteps
           (topicPrior 2 fixed_vocab >>= topicModel {vocab_size_sub1 = 3} fixed_vocab dataset)
  -- print xs
  pure ()

||| SMC
export
smcTopic : Nat -> Nat -> IO ()
smcTopic n_particles n_words = do
  dataset <- mkTopicData n_words
  let n_timesteps = n_particles
  xs <- sampleIO $ runPopulation $ smc n_timesteps n_particles
            (topicPrior fixed_n_topics fixed_vocab >>= topicModel {vocab_size_sub1 = 3} fixed_vocab dataset)
  -- print xs
  pure ()

||| RMSMC
export
rmsmcTopic : Nat -> Nat -> Nat -> IO ()
rmsmcTopic n_particles n_mhsteps n_words = do
  dataset <- mkTopicData n_words
  let n_timesteps = n_particles
  xs <- sampleIO $ runPopulation $ rmsmc n_timesteps n_particles n_mhsteps
            (topicPrior fixed_n_topics fixed_vocab >>= topicModel {vocab_size_sub1 = 3}  fixed_vocab dataset)
  -- print xs
  pure ()
