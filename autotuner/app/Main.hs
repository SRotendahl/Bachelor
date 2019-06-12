module Main where 

import System.Environment
import Tuning

main = do
  args <- getArgs
  res <- tune (args !! 0) (args !! 1) (read $ args !! 2)
  print . head $ res
