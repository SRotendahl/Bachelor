module Tuning where

import Tree
import JsonParser
import Regex

import Data.Tree
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

import System.IO.Silently
import System.Process
import System.Directory

import qualified Data.ByteString.Lazy as BS

-- Needs to be removed TODO
--import Text.Regex
--import Text.Regex.Base

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
    
-------------- Tuning ---------------- 

-- This function should be changed if upper/lower bound is a thing
setThresholdVal :: [(String, Integer)] -> (String, Bool) -> Integer
setThresholdVal comps thres = 
  let val = fromJust $ lookup (fst thres) comps
  in if snd thres then val else val + 1

tuneThresSrt :: String -> Integer -> String
tuneThresSrt name val = name ++ '=':(show val) ++ ","

createTuneStr :: [(String,Integer)] -> [(String,Bool)] -> String
createTuneStr comps = concat . (map (\(n,b) -> tuneThresSrt n $ setThresholdVal comps (n,b)))
  

-- Get runtime of a single execution of a single program
{-
tuneProgram :: [(String, Int)] -> [(String, Bool)] -> String -> String -> IO Float
tuneProgram comps exe ext progName = do
  let tunePara = createTuneFile comps exe
  writeFile (progName ++ ('.':ext)) tunePara
  benchOut <- getBenchOutput progName ext
  removeFile (progName ++ ('.':ext))
  let nameTime = splitNameTime $ lines (fst benchOut)
  return $ snd (head nameTime)
-}

hackyAverage :: (String,[[(String,[Integer])]]) ->  (String,[Integer])
hackyAverage (ds, runInfo) =
  let runtimes = map (concat . (map snd)) runInfo
  in  (ds, map (foldr (+) 0) runtimes)

--tune :: String -> [[(String,Bool)]] -> [(String,[(String,Integer)])] -> IO (String,[(String,Bool)], Integer)
tune progName paths comps = do
  runInfo <- runtimeAll progName paths comps 
  let (ds,pathTimes) = unzip $ map hackyAverage runInfo 
  let pathsWithTimes = map (zip paths) pathTimes
  let tmp = map (\(xs,z) -> map (\(x,y) -> (z,x,y)) xs) $ zip pathsWithTimes ds
  let tmp2 = sortBy (\(_,_,a) (_,_,b) -> compare a b) $ concat tmp
  return . head $ tmp2

-- Runtimes --
runtimeDataset :: String -> [(String,Integer)] -> [[(String,Bool)]] -> IO [[(String,[Integer])]]
runtimeDataset progName comps = (mapM (benchRuntimes progName . (createTuneStr comps)))

runtimeAll :: String -> [[(String,Bool)]] -> [(String,[(String,Integer)])] -> IO [(String,[[(String,[Integer])]])]
runtimeAll progName paths dsInfo = do
  let (names, comps) = unzip dsInfo
  runtimes <- mapM (flip (runtimeDataset progName) paths) comps
  return $ zip names runtimes

-- Helper functions --
handleJsonFile :: String -> String -> (BS.ByteString -> a) -> IO a
handleJsonFile tmpName compCmd f = do
  silence $ callCommand compCmd 
  jsonDump <- BS.readFile tmpName
  let res = f jsonDump 
  removeFile tmpName
  return res

-- Get runtimes of a specific execution in one dataset  (TODO: update this so it's smarter) --
extractRuntimes :: [(String, [(String, Dataset)])] -> [(String, [Integer])]
extractRuntimes ((_,x):[]) = map (\(n,d) -> (n, runtimes d)) x
extractRuntimes _ =
  error "Json contains multiple programs, this is not currently supported."

numRuns = 2

benchRuntimes ::  String -> String -> IO [(String, [Integer])]
benchRuntimes progName tuneCont = do
  let tuneFileName = (progName ++ ".tuning")
  writeFile tuneFileName tuneCont
  let tmpName = ".tmpJsonRuntimes,json"
  let cmd = "futhark bench --backend=opencl -r " ++ show numRuns ++ " " ++
             progName ++ " --skip-compilation --exclude-case=notune --json="
             ++ tmpName
  res <- handleJsonFile tmpName cmd (extractRuntimes . parseBenchJson)
  removeFile tuneFileName
  return res

-- Get comparison values and threshold structure --
extractComps :: [(String, [(String, Dataset)])] -> [(String, [(String, Integer)])]
extractComps ((_,x):[]) = map (\(n,d) -> (n, nub . getComparison . err $ d)) x
extractComps _ =
  error "Json contains multiple programs, this is not currently supported."

getComps :: String -> String -> IO [(String, [(String, Integer)])]
getComps backend progName = do
  let tmpName = ".tmpBenchOutput.json"
  let cmd = "futhark bench --backend=opencl -p -L  " ++ progName ++ 
            " --skip-compilation --exclude-case=notune --json="
            ++ tmpName
  handleJsonFile tmpName cmd $ extractComps . parseBenchJson 

fileNameToExe :: String -> String
fileNameToExe fileName = ("./" ++) . head . (splitOn ".fut") $ fileName

getStructure :: String -> IO (Tree (String,Bool))
getStructure prog = do
  sizes <- readProcess (fileNameToExe prog) ["--print-sizes"] ""
  return . buildTree . getTresh $ sizes

{-
 - first plan:
 -- get runtime of path with dataset
 -- map over all paths
 -- map over all datasets
 -- get average of all times
 -- pick best average
 -- return path
 - extentions: 
 -- create and Eq instance for paths so we can know if they are equal across datasets
 -- figure out upper/lower bounding of numbers to optimize.
-}


------- TODO needs to be deleted ---------
{-
splitNameTime ::[String] -> [(String,Float)]
splitNameTime str =
  let regex = mkRegex "dataset +([^:]+)\\:\\ +([0-9]+\\.[0-9]+)"
      list  = catMaybes $ map (matchRegex regex) str
  in  map (\x -> (x!!0, read $ x!!1)) list
-}
