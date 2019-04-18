module Tree (buildTree, printTree) where

import Data.List
import Data.Tree

{- TODO
 - Alias (String,[String]) to a better name
-}

toTree :: [(String,[String])] -> [String] -> (String, [[String]])
-- nullRoot is simply to make sure that if no thresholds are found a tree is 
-- still made
toTree thress [] = 
  ("nullRoot", roots) 
  where roots = map ((:[]) . fst) $ filter (null . snd) thress 
toTree thress prevThress =
  (currThres, nextThress)
  where currThres  = head prevThress 
        nextThress = map ((:prevThress) . fst) $ -- Should be it's own function
                     filter (((==) $ sort prevThress) . sort . snd) thress

buildTree :: [(String, [String])] -> (Tree String)
buildTree threshs = unfoldTree (toTree threshs) []

printTree = drawTree
