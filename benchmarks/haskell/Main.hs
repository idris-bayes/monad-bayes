module Main where

import Benchmarks
    ( bench_LR,
      bench_HMM,
      bench_Topic,
      bench_MH,
      bench_SMC,
      bench_RMSMC )
import System.Environment (getArgs)

main :: IO ()
main = do
  bench_LR
  bench_HMM
  bench_Topic
  bench_MH
  bench_SMC
  bench_RMSMC