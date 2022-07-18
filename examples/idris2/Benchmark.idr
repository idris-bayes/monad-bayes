module Benchmark

import System.File.ReadWrite
import System.Clock
import Data.List
import LinRegr
import HMM
import Topic

fileName : String
fileName = "benchmarks.csv"

||| Write a line prepended with '\n' to a file,
appendFileLn : String -> String -> IO ()
appendFileLn file_name line = do
  _ <- appendFile file_name ("\n" ++ line)
  pure ()

||| Write a CSV row to a file, where 'label' is the first column and 'values' are the rest
writeRow : Show a => String -> (String, List a) -> IO ()
writeRow file_name (label, values) = do
  let line : List (String)
      line = (label :: map show values)
  appendFileLn file_name (concat $ intersperse "," line)

||| Execute a program once and return the duration in seconds
benchmark : IO a -> IO Double
benchmark prog = do
  t1     <- clockTime UTC
  _      <- prog
  t2     <- clockTime UTC
  let duration_ns : Integer
      duration_ns = nanoseconds (timeDifference t2 t1)

      duration_s : Double
      duration_s = (cast duration_ns) / (cast 1000000000)
  print ("Duration: " ++ show duration_s)
  pure duration_s

||| Execute a program across a range of parameters and write row to file
benchRow :
  -- | (Program name, program)
     (String, Nat -> IO a)
  -- | (Independent variable name, values)
  -> (String, List Nat)
  -- | List of run-times
  -> IO ()
benchRow (prog_name, prog) (param_name, params) = do
  print ("Running " ++ prog_name ++ " over " ++ param_name ++ " for values " ++ show params)
  -- Run program over varying parameter values and write.
  -- e.g. "LinRegr-MH100, 0.23, 0.87, 1.23, 1.78, 2.45"
  means <- sequence $ map (benchmark . prog) params
  writeRow fileName (prog_name, means)

{- | Varying over dataset size -}
fixed_mh_steps : Nat
fixed_mh_steps = 2000
fixed_smc_particles : Nat
fixed_smc_particles = 100
fixed_rmsmc_particles : Nat
fixed_rmsmc_particles = 10
fixed_rmsmc_mh_steps : Nat
fixed_rmsmc_mh_steps = 1

export
bench_LR : IO ()
bench_LR = do
    let row_header = ("Dataset size", [200, 400, 600, 800, 1000])
    writeRow fileName row_header
    benchRow ("LR-MH100", mhLinRegr fixed_mh_steps) row_header
    benchRow ("LR-SMC100", smcLinRegr fixed_smc_particles) row_header
    benchRow ("LR-RMSMC10-1", rmsmcLinRegr fixed_rmsmc_particles fixed_mh_steps) row_header

export
bench_HMM : IO ()
bench_HMM = do
    let row_header = ("Dataset size", [200, 400, 600, 800, 1000])
    writeRow fileName row_header
    benchRow ("HMM-MH100", mhHMM fixed_mh_steps) row_header
    benchRow ("HMM-SMC100", smcHMM fixed_smc_particles) row_header
    benchRow ("HMM-RMSMC10-1", rmsmcHMM fixed_rmsmc_particles fixed_mh_steps) row_header

export
bench_Topic : IO ()
bench_Topic = do
    let row_header = ("Dataset size", [500, 1000, 1500, 2000, 2500])
    writeRow fileName row_header
    benchRow ("Topic-MH100", mhTopic fixed_mh_steps) row_header
    benchRow ("Topic-SMC100", smcTopic fixed_smc_particles) row_header
    benchRow ("Topic-RMSMC10-1", rmsmcTopic fixed_rmsmc_particles fixed_mh_steps) row_header

{- | Varying over inference parameters -}
fixed_lr_datasize : Nat
fixed_lr_datasize = 50
fixed_hmm_datasize : Nat
fixed_hmm_datasize = 20
fixed_topic_datasize : Nat
fixed_topic_datasize = 50

export
bench_MH : IO ()
bench_MH = do
    let row_header = ("Number of steps", [200, 400, 600, 800, 1000])
    benchRow ("LR50-MH", flip mhLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-MH", flip mhHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-MH", flip mhTopic fixed_topic_datasize) row_header

export
bench_SMC : IO ()
bench_SMC = do
    let row_header = ("Number of particles", [200, 400, 600, 800, 1000])
    benchRow ("LR50-SMC", flip smcLinRegr fixed_lr_datasize) row_header
    benchRow ("HMM20-SMC", flip smcHMM fixed_hmm_datasize) row_header
    benchRow ("Topic50-SMC", flip smcTopic fixed_topic_datasize) row_header

export
bench_RMSMC : IO ()
bench_RMSMC = do
    let row_header = ("Number of rejuv steps", [20, 40, 60, 80, 100])
    benchRow ("LR50-RMSMC10", \rejuv_steps => rmsmcLinRegr fixed_rmsmc_particles rejuv_steps fixed_lr_datasize) row_header
    benchRow ("HMM20-RMSMC10", \rejuv_steps => rmsmcHMM fixed_rmsmc_particles rejuv_steps fixed_hmm_datasize) row_header
    benchRow ("Topic50-RMSMC10", \rejuv_steps => rmsmcTopic fixed_rmsmc_particles rejuv_steps fixed_topic_datasize) row_header


main : IO ()
main = do
  bench_LR
  bench_HMM
  bench_Topic
  bench_MH
  bench_SMC
  bench_RMSMC

{-
pack --with-ipkg examples.ipkg repl Benchmark.idr
-}

