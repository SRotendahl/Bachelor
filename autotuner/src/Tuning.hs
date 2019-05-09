module Tuning where

import Tree
import Data.Tree
import Data.List

import System.IO.Silently
import Data.Maybe
import System.Process

-- Needs to be removed TODO
import Text.Regex
import Text.Regex.Base

--------- Get execution paths -----------
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
    
-------------- Tuning ---------------- TODO, Better names

checkExePath :: (String, Bool) -> [(String, Int)] -> Int
checkExePath exe comps = --BeterName
  let path = lookup (fst exe) comps
      thres = fromJust path
  in if snd exe then thres else thres + 1


getBenchOutput progName tuneExt =
  let cmd = "futhark bench --backend=opencl " ++ progName ++ 
            " --tuning=" ++ tuneExt ++ " --skip-compilation --exclude-case=notune"
  in capture $ callCommand cmd

createTuneFile :: [(String, Int)] -> [(String, Bool)] -> String
createTuneFile comps exes =
  let strList = map (\exe -> (fst exe) ++ "=" ++ show (checkExePath exe comps)++",") exes
  in concat strList

tuneProgram :: [(String, Int)] -> [(String, Bool)] -> String -> String -> IO Float
tuneProgram comps exe ext progName = do
  let tunePara = createTuneFile comps exe
  writeFile (progName ++ ('.':ext)) tunePara
  benchOut <- getBenchOutput progName ext
  let nameTime = splitNameTime $ lines (fst benchOut)
  return $ snd (head nameTime)

------- TODO needs to be deleted ---------
splitNameTime ::[String] -> [(String,Float)]
splitNameTime str =
  let regex = mkRegex "dataset +([^:]+)\\:\\ +([0-9]+\\.[0-9]+)"
      list  = catMaybes $ map (matchRegex regex) str
  in  map (\x -> (x!!0, read $ x!!1)) list
