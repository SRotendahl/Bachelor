module Tree where

import Data.List
import Data.Tree
import Data.Function
import Data.Maybe

{- TODO
 - Alias (String,[String]) to a better name
-}

toTree :: [((String,Bool),[String])] -> [(String,Bool)] -> ((String,Bool), [[(String,Bool)]])
-- nullRoot is simply to make sure that if no thresholds are found a tree is 
-- still made
toTree thress [] = 
  (("nullRoot",False), roots) 
  where roots = map ((:[]) . fst) $ filter (null . snd) thress 
toTree thress prevThress =
  (currThres, nextThress)
  where currThres  = head prevThress 
        nextThress = map ((:prevThress) . fst) $ -- Should be it's own function
                     filter (((==) $ sort . map fst $ prevThress) . sort . snd) thress

checkTHs :: [(String, [(String, Bool)])] -> [((String,Bool), [String])]
checkTHs ths = 
  let thBls = map ((\(a,b) -> (head a,b)) . unzip)
            . groupBy ((==) `on` fst) . sort . concat . map snd $ ths 
      pred = and . map ((==) 2 . length . snd) $ thBls 
      res = map (\(n,ts) -> ((,) n . head . fromMaybe [False] . lookup n $ thBls, map fst ts)) ths
  in  if pred then error "Multiple parents not supported" else res 
      
      
buildTree :: [(String, [(String,Bool)])] -> (Tree (String, Bool))
--TODO alias stuff
buildTree threshs = 
  unfoldTree (toTree $ checkTHs threshs) [] 
                    
printTree :: (Show a) => Tree a -> String
printTree = drawTree . fmap show 
