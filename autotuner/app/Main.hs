module Main where 

import Tuning

main = do
  a <-  getStructure "test/LocVolCalib.fut"
  b <- getComps "opencl" "test/LocVolCalib.fut"
  res <- tune "test/LocVolCalib.fut" (getExecutions a) b
  print $ res
