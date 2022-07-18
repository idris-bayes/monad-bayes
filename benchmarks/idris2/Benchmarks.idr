module Benchmarks

import System.File.ReadWrite
import System.Clock
import Data.List

fileName : String
fileName = "benchmarks.csv"

appendFileLn : String -> String -> IO ()
appendFileLn file_name line = do
  _ <- appendFile file_name ("\n" ++ line)
  pure ()

writeRow : Show a => String -> (String, List a) -> IO ()
writeRow file_name (label, values) = do
  let line : List (String)
      line = (label :: map show values)
  appendFileLn file_name (concat $ intersperse "," line)


-- benchMean : IO a -> IO Double
-- benchMean prog = do
--   t1     <- clockTime GCReal
--   report <- prog
--   t2     <- clockTime GCReal
--   let -- duration : Clock GCReal
--       duration = nanoseconds t1
--   pure 0