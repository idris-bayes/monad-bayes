{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use camelCase" #-}
module Benchmark where

import LinRegr
import HMM
import Topic

import Criterion.Main
import Criterion.Types
import Data.List
import Statistics.Types
import Control.DeepSeq
import Criterion (benchmark')

{- | Benchmarking utility
-}
fileName :: String
fileName = "benchmarks.csv"

appendFileLn :: String -> String -> IO ()
appendFileLn file_name = appendFile file_name . ("\n" ++ )

writeRow :: Show a => String -> (String, [a]) -> IO ()
writeRow file_name (label, values) = appendFileLn file_name (intercalate "," (label : map show values))

benchMean :: NFData a => IO a -> IO Double
benchMean prog = do
  report <- benchmark' (nfIO prog)
  let analysis = reportAnalysis report
      estMean  = estPoint (anMean analysis)
  return estMean

benchRow :: NFData a
  -- | (Program name, program)
  => (String, Int -> IO a)
  -- | (Independent variable name, values)
  -> (String, [Int])
  -- | List of run-times
  -> IO ()
benchRow (prog_name, prog) (_, params) = do
  -- Run program over varying parameter values and write e.g. "LinRegr-MH100, 0.23, 0.87, 1.23, 1.78, 2.45"
  means <- mapM (benchMean . prog) params
  writeRow fileName (prog_name, means)

{- | Varying over dataset size
-}
fixed_mh_steps :: Int
fixed_mh_steps = 2000
fixed_smc_particles :: Int
fixed_smc_particles = 100
fixed_rmsmc_particles :: Int
fixed_rmsmc_particles = 10
fixed_rmsmc_mh_steps :: Int
fixed_rmsmc_mh_steps = 1

bench_LR :: IO ()
bench_LR = do
    let row_header = ("Dataset size", [200, 400, 600, 800, 1000])
    writeRow fileName row_header
    benchRow ("LR-MH100", mhLinRegr fixed_mh_steps) row_header
    benchRow ("LR-SMC100", smcLinRegr fixed_smc_particles) row_header
    benchRow ("LR-RMSMC10-1", rmsmcLinRegr fixed_rmsmc_particles fixed_mh_steps) row_header

bench_HMM :: IO ()
bench_HMM = do
    let row_header = ("Dataset size", [200, 400, 600, 800, 1000])
    writeRow fileName row_header
    benchRow ("HMM-MH100", mhHMM fixed_mh_steps) row_header
    benchRow ("HMM-SMC100", smcHMM fixed_smc_particles) row_header
    benchRow ("HMM-RMSMC10-1", rmsmcHMM fixed_rmsmc_particles fixed_mh_steps) row_header

bench_Topic :: IO ()
bench_Topic = do
    let row_header = ("Dataset size", [500, 1000, 1500, 2000, 2500])
    writeRow fileName row_header
    benchRow ("Topic-MH100", mhTopic fixed_mh_steps) row_header
    benchRow ("Topic-SMC100", smcTopic fixed_smc_particles) row_header
    benchRow ("Topic-RMSMC10-1", rmsmcTopic fixed_rmsmc_particles fixed_mh_steps) row_header

{- | Varying over inference parameters
-}
fixed_lr_datasize :: Int
fixed_lr_datasize = 50
fixed_hmm_datasize :: Int
fixed_hmm_datasize = 20
fixed_topic_datasize :: Int
fixed_topic_datasize = 50

bench_MH :: IO ()
bench_MH = do
    let row_header = ("Number of steps", [200, 400, 600, 800, 1000])
    benchRow ("LR50-MH", flip mhLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-MH", flip mhHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-MH", flip mhTopic fixed_topic_datasize) row_header

bench_SMC :: IO ()
bench_SMC = do
    let row_header = ("Number of particles", [200, 400, 600, 800, 1000])
    benchRow ("LR50-SMC", flip smcLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-SMC", flip smcHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-SMC", flip smcTopic fixed_topic_datasize) row_header

bench_RMSMC :: IO ()
bench_RMSMC = do
    let row_header = ("Number of rejuv steps", [20, 40, 60, 80, 100])
    benchRow ("LR50-RMSMC10", \rejuv_steps -> rmsmcLinRegr fixed_rmsmc_particles rejuv_steps fixed_lr_datasize) row_header
    benchRow ("HMM20-RMSMC10", \rejuv_steps -> rmsmcHMM fixed_rmsmc_particles rejuv_steps fixed_hmm_datasize) row_header
    benchRow ("Topic50-RMSMC10", \rejuv_steps -> rmsmcTopic fixed_rmsmc_particles rejuv_steps fixed_topic_datasize) row_header

benchmark :: IO ()
benchmark = do
  bench_LR
  bench_HMM
  bench_Topic
  bench_MH
  bench_SMC
  bench_RMSMC