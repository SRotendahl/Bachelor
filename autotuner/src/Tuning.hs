module Tuning where

{- Plan:
 - All thresholds start at inf
 - Move through tree recursively:
 - - set threshold to be the number 
 - - run program and save runtime
 - - - might be an idea to create a tree with thresholds and runtimes
 - - - and one with thresholds and values
 - Find the node with the best runtime and return the params to achive 
 - that execution path.
 -}

tune progName = 0
