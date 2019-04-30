module Tuning where

import Tree
import Data.Tree
import Data.List

fixSplit :: [String] -> [[(String,Bool)]] -> [[(String,Bool)]]
fixSplit [] childExecu = childExecu
fixSplit [c] childExecu = 
  let ce = filterName c
  in  concat $ map (\p1 -> (:) <$> p1 <*> childExecu) ce
  where filterName n = filter ((==n) . fst . head) childExecu
fixSplit (c1:c2:cs) childExecu =
  let ce1 = filterName c1
      ce2 = filterName c2
      newce = childExecu \\ (ce1 ++ ce2)
      paths = [x++y | x <- ce2, y <- ce1]
  in  fixSplit cs $ newce ++ paths
  where filterName n = filter ((==n) . fst . head) childExecu

getAllBPaths :: Bool -> [(Tree (String, Bool))] -> [[(String,Bool)]]
getAllBPaths b children = 
  let childrenB = filter ((==b) . snd . rootLabel) children in
  case childrenB of
    []  -> []
    [c] -> concat $ map getExecutions childrenB 
    cs  -> let childrenExecuB = concat $ map getExecutions cs
               names = map (fst . rootLabel) childrenB
           in  fixSplit names childrenExecuB

addLeaf :: (String,Bool) -> [[(String,Bool)]] -> [[(String,Bool)]]
addLeaf tup [] = [[tup]]
addLeaf _ lst = lst

getExecutions :: (Tree (String, Bool)) -> [[(String,Bool)]]
getExecutions (Node{rootLabel=("nullRoot",_), subForest=children}) =
  concat $ map getExecutions children
getExecutions (Node{rootLabel=(name,_), subForest=[]}) =
  [[(name,True)],[(name,False)]]
getExecutions (Node{rootLabel=(name,_), subForest=children}) =
  let pathsT = addLeaf (name,True) $ (:) <$> [(name,True)] <*> (getAllBPaths True children)
      pathsF = addLeaf (name,False) $ (:) <$> [(name,False)] <*> (getAllBPaths False children)
  in  pathsF ++ pathsT
    
{-
createParams :: Maybe (String, Maybe Int) -> [(String, Maybe Int)] -> String
createParams trueTH falseTHs =
 -} 
