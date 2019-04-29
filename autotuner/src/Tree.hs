module Tree (buildTree, printTree) where

import Data.List
import Data.Tree
import ParsingJson

{- TODO
 - Alias (String,[String]) to a better name
 - Create a function to make tree with numbers
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

addCompVals :: [(String,Int)] -> String -> (String, (Maybe Int))
addCompVals ds thres = (thres, val)
  where val = lookup thres ds

buildTree :: [(String,Int)] -> [(String, [String])] -> (Tree (String, (Maybe Int))) --TODO alias stuff
buildTree dataset threshs = fmap (addCompVals dataset)
                    $ unfoldTree (toTree threshs) []
                    
--buildForest :: [(String, [(String,Int)]] -> [(String, [String])]-> [(Tree (String, (Maybe Int)))]
-- For later

printTree = drawTree
