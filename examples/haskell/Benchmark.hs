{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
module Benchmark where

import LinRegr
import HMM
import Topic

import Criterion.Main
import Criterion.Types
import Data.List
import Data.List.Split
import Statistics.Types
import Control.DeepSeq
import Criterion (benchmark')

{- | Benchmarking utility
-}
fixed_fileName :: String
fixed_fileName = "benchmarks.csv"

appendFileLn :: String -> String -> IO ()
appendFileLn file_name = appendFile file_name . (++ "\n")

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
  writeRow fixed_fileName (prog_name, means)

{- | Varying over dataset size
-}
fixed_mh_steps :: Int
fixed_mh_steps = 100
fixed_smc_particles :: Int
fixed_smc_particles = 100
fixed_rmsmc_particles :: Int
fixed_rmsmc_particles = 10
fixed_rmsmc_mh_steps :: Int
fixed_rmsmc_mh_steps = 1

bench_LR :: [Int] -> IO ()
bench_LR args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_fileName row_header
    benchRow ("LR-MH100", mhLinRegr fixed_mh_steps) row_header
    benchRow ("LR-SMC100", smcLinRegr fixed_smc_particles) row_header
    benchRow ("LR-RMSMC10-1", rmsmcLinRegr fixed_rmsmc_particles fixed_mh_steps) row_header

bench_HMM :: [Int] -> IO ()
bench_HMM args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_fileName row_header
    benchRow ("HMM-MH100", mhHMM fixed_mh_steps) row_header
    benchRow ("HMM-SMC100", smcHMM fixed_smc_particles) row_header
    benchRow ("HMM-RMSMC10-1", rmsmcHMM fixed_rmsmc_particles fixed_mh_steps) row_header

bench_Topic :: [Int] -> IO ()
bench_Topic args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_fileName row_header
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

bench_MH :: [Int] -> IO ()
bench_MH args = do
    let row_header = ("Number of steps", args)
    writeRow fixed_fileName row_header
    benchRow ("MH-LR50", flip mhLinRegr fixed_lr_datasize) row_header
    benchRow ("MH-HMM20", flip mhHMM fixed_hmm_datasize) row_header
    benchRow ("MH-Topic50", flip mhTopic fixed_topic_datasize) row_header

bench_SMC ::  [Int] -> IO ()
bench_SMC args = do
    let row_header = ("Number of particles", args)
    writeRow fixed_fileName row_header
    benchRow ("SMC-LR50", flip smcLinRegr fixed_lr_datasize) row_header
    benchRow ("SMC-HMM20", flip smcHMM fixed_hmm_datasize) row_header
    benchRow ("SMC-Topic50", flip smcTopic fixed_topic_datasize) row_header

bench_RMSMC ::  [Int] -> IO ()
bench_RMSMC args = do
    let row_header = ("Number of rejuv steps", args)
    writeRow fixed_fileName row_header
    benchRow ("RMSMC10-LR50", \rejuv_steps -> rmsmcLinRegr fixed_rmsmc_particles rejuv_steps fixed_lr_datasize) row_header
    benchRow ("RMSMC10-HMM20", \rejuv_steps -> rmsmcHMM fixed_rmsmc_particles rejuv_steps fixed_hmm_datasize) row_header
    benchRow ("RMSMC10-Topic50", \rejuv_steps -> rmsmcTopic fixed_rmsmc_particles rejuv_steps fixed_topic_datasize) row_header

runBenchmarks :: IO ()
runBenchmarks = do
  -- | Read input benchmark parameters
  content <- readFile "../benchmark_params.txt"
  let removeComments :: [String] -> [String]
      removeComments = filter (\case []     -> False
                                     (x:xs) -> x /= '#')
  let args :: [[Int]]
      args = map (map read . splitOn ",") (removeComments (lines content))
  -- | Run benchmark programs on their corresponding parameters
  case args of
        [lr, hmm, topic, mh, smc, rmsmc] -> do
          bench_LR lr
          bench_HMM hmm
          bench_Topic topic
          bench_MH mh
          bench_SMC smc
          bench_RMSMC rmsmc
        _   -> error "bad input file"