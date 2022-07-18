module Main where

import BenchmarkTests
import System.Environment (getArgs)

main :: IO ()
main = do
 benchDataSize

benchDataSize = do
  benchmarkLinRegrMH_DataSize
  benchmarkHMMMH_DataSize
  benchmarkTopicMH_DataSize
