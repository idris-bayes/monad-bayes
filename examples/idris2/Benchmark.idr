module Benchmark

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
import System.File.ReadWrite
import System.Clock
import Data.List
import Data.String
import LinRegr
import HMM
import Topic

fixed_fileName : String
fixed_fileName = "benchmarks.csv"

||| Write a line prepended with '\n' to a file,
appendFileLn : String -> String -> IO ()
appendFileLn file_name line = do
  _ <- appendFile file_name (line ++ "\n")
  pure ()

||| Write a CSV row to a file, where 'label' is the first column and 'values' are the rest
writeRow : Show a => String -> (String, List a) -> IO ()
writeRow file_name (label, values) = do
  let line : List (String)
      line = (label :: map show values)
  appendFileLn file_name (concat $ intersperse "," line)

||| Execute a program once and return the duration in seconds
benchmarkOnce : IO a -> IO Double
benchmarkOnce prog = do
  t1     <- clockTime UTC
  _      <- prog
  t2     <- clockTime UTC
  let duration_ns : Integer
      duration_ns = nanoseconds (timeDifference t2 t1)

      duration_s : Double
      duration_s = (cast duration_ns) / (cast 1000000000)
  pure duration_s

||| Execute a program a certain amount of iterations and return the mean duration in seconds
benchmark : String -> IO a -> IO Double
benchmark comment prog = do
  let iterations = 8
  times <- sequence (List.replicate iterations $ benchmarkOnce prog)
  let mean = (sum times) / (cast iterations)
  putStrLn "\{comment} (Duration: \{show mean}s)"
  pure mean

||| Execute a program across a range of parameters and write row to file
benchRow :
  -- | (Program name, program)
     (String, Nat -> IO a)
  -- | (Independent variable name, values)
  -> (String, List Nat)
  -- | List of run-times
  -> IO ()
benchRow (prog_name, prog) (param_name, params) = do
  putStrLn ("Running " ++ prog_name ++ " with " ++ param_name ++ " for values " ++ show params)
  -- Run program over varying parameter values and write.
  -- e.g. "LinRegr-MH100, 0.23, 0.87, 1.23, 1.78, 2.45"
  means <- sequence $ map (\param => let param_str = show param
                                     in  benchmark ("\{prog_name}, \{param_name} = \{param_str}") (prog param) ) params
  writeRow fixed_fileName (prog_name, means)

{- | Varying over dataset size -}
fixed_mh_steps : Nat
fixed_mh_steps = 100
fixed_smc_particles : Nat
fixed_smc_particles = 100
fixed_rmsmc_particles : Nat
fixed_rmsmc_particles = 8
fixed_rmsmc_mh_steps : Nat
fixed_rmsmc_mh_steps = 1

export
bench_LR : List Nat -> IO ()
bench_LR params = do
    let row_header = ("Dataset size", params)
    writeRow fixed_fileName row_header
    benchRow ("LR-MH100", mhLinRegr fixed_mh_steps) row_header
    benchRow ("LR-SMC100", smcLinRegr fixed_smc_particles) row_header
    benchRow ("LR-RMSMC8-1", rmsmcLinRegr fixed_rmsmc_particles fixed_mh_steps) row_header

export
bench_HMM : List Nat -> IO ()
bench_HMM params = do
    let row_header = ("Dataset size", params)
    writeRow fixed_fileName row_header
    benchRow ("HMM-MH100", mhHMM fixed_mh_steps) row_header
    benchRow ("HMM-SMC100", smcHMM fixed_smc_particles) row_header
    benchRow ("HMM-RMSMC8-1", rmsmcHMM fixed_rmsmc_particles fixed_mh_steps) row_header

export
bench_Topic : List Nat -> IO ()
bench_Topic params = do
    let row_header = ("Dataset size", params)
    writeRow fixed_fileName row_header
    benchRow ("Topic-MH100", mhTopic fixed_mh_steps) row_header
    benchRow ("Topic-SMC100", smcTopic fixed_smc_particles) row_header
    benchRow ("Topic-RMSMC8-1", rmsmcTopic fixed_rmsmc_particles fixed_mh_steps) row_header

{- | Varying over inference parameters -}
fixed_lr_datasize : Nat
fixed_lr_datasize = 50
fixed_hmm_datasize : Nat
fixed_hmm_datasize = 20
fixed_topic_datasize : Nat
fixed_topic_datasize = 50

export
bench_MH : List Nat -> IO ()
bench_MH params = do
    let row_header = ("Number of steps", params)
    writeRow fixed_fileName row_header
    benchRow ("LR50-MH", flip mhLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-MH", flip mhHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-MH", flip mhTopic fixed_topic_datasize) row_header

export
bench_SMC : List Nat -> IO ()
bench_SMC params = do
    let row_header = ("Number of particles", params)
    writeRow fixed_fileName row_header
    benchRow ("LR50-SMC", flip smcLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-SMC", flip smcHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-SMC", flip smcTopic fixed_topic_datasize) row_header

export
bench_RMSMC : List Nat -> IO ()
bench_RMSMC params = do
    let row_header = ("Number of rejuv steps", params)
    writeRow fixed_fileName row_header
    benchRow ("LR50-RMSMC8", \rejuv_steps => rmsmcLinRegr fixed_rmsmc_particles rejuv_steps fixed_lr_datasize) row_header
    benchRow ("HMM20-RMSMC8", \rejuv_steps => rmsmcHMM fixed_rmsmc_particles rejuv_steps fixed_hmm_datasize) row_header
    benchRow ("Topic50-RMSMC8", \rejuv_steps => rmsmcTopic fixed_rmsmc_particles rejuv_steps fixed_topic_datasize) row_header

fixed_numParams : Nat
fixed_numParams = 5

runBenchmarks : IO ()
runBenchmarks = do
  -- | Read input benchmark parameters
  Right content <- readFile "benchmark_params.txt"
    | _ => pure ()
  -- | Group into 6 lists of 5 values
  let chunksOf : Nat -> List a -> List (List a)
      chunksOf _ [] = []
      chunksOf n xs = (take n xs) :: (chunksOf n (drop n xs))
  let args : List (List Nat)
      args = chunksOf fixed_numParams (map cast (lines content))
  -- | Run benchmark programs on their corresponding parameters
  case args of
        (lr :: hmm :: topic :: mh :: smc :: rmsmc :: []) => do
          bench_LR lr
          bench_HMM hmm
          bench_Topic topic
          bench_MH mh
          bench_SMC smc
          bench_RMSMC rmsmc
        _   => assert_total (idris_crash "bad input file")

{-
pack --with-ipkg examples.ipkg repl Benchmark.idr
-}

