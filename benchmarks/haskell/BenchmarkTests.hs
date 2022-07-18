{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use camelCase" #-}
module BenchmarkTests where

import LinRegr
import HMM
import Topic

import Criterion.Main
import Criterion.Types
import Data.Bifunctor
import Control.DeepSeq

configFile = defaultConfig {csvFile = Just "benchmarks.csv"}

benchmark :: forall a. NFData a
  => String
  -> (Int -> Int -> IO a)
  -> [(String, (Int, Int))]
  -> IO ()
benchmark groupName benchmarkProg params = do
  defaultMainWith configFile
    [bgroup groupName
      [ bench label (nfIO $  (benchmarkProg sample_size data_size ))
      | (label, (sample_size, data_size)) <- params  ]
    ]

{- Varying over dataset size -}

benchmarkLinRegrMH_DataSize = do
    let sample_size = 2000
    benchmark "linRegr/MH/data-size" mhLinRegr
       [(show data_size, (sample_size, data_size)) | data_size <- [200, 400, 600, 800, 1000]]

benchmarkHMMMH_DataSize = do
    let sample_size = 2000
    benchmark "hmm/MH/data-size" mhHMM
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]

benchmarkTopicMH_DataSize = do
    let sample_size = 2000
    benchmark "topic/MH/data-size" mhTopic
       [(show data_size, (sample_size, data_size)) | data_size <- [40, 80, 120, 160, 200]]
