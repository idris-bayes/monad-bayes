module Main

import Benchmarks

main : IO ()
main = do
  bench_LR
  bench_HMM
  bench_Topic
  bench_MH
  bench_SMC
  bench_RMSMC