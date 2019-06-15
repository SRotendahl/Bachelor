module Main where 

import System.Environment
import Tuning

main = do
  args <- getArgs
  printAndSaveTuning (args !! 0) (args !! 1) (read $ args !! 2) (args !! 3)
