module Tuning where

import Tree
import JsonParser
import Regex

import Data.Ord
import Data.Function
import Data.Tree
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

import System.IO.Silently
import System.Process
import System.Directory

import qualified Data.ByteString.Lazy as BS

combineTHvals :: [(String, Integer)] -> [(String, [Integer])]
combineTHvals = map (\l -> (fst . head $ l, nub $ map snd l))
                . groupBy ((==) `on` fst) . sortBy (comparing fst)

addFalseVal :: [Integer] -> [Integer]
addFalseVal xs = xs ++ (((+1) . last $ xs):[])

-- (THName, (AlwaysTrue, Both, AlwaysFalse))
allTHvals :: [(String, [(String, Integer)])]
          -> [(String, [Integer])]
allTHvals = map (\th -> (fst th, addFalseVal . sort . snd $ th))
            . combineTHvals . concat . map snd

listFromMaybe :: Maybe [a] -> [a]
listFromMaybe Nothing = []
listFromMaybe (Just xs) = xs

addTHvalsToTree :: [(String, [Integer])] -> (Tree (String, Bool)) -> (Tree (String, Bool, [Integer]))
addTHvalsToTree thVals = fmap (\(name, bool) -> (name, bool, listFromMaybe $ lookup name thVals))

type Path = [(String, Integer)]
type Runtimes = [(String, [Integer])]

-- Fold Tree to list of paths --
-- Could be a map, but might look weird
combinePaths :: [[Path]] -> [Path]
combinePaths = foldl (\acc elm -> (++) <$> acc <*> elm) [[]]

filterAndCombine :: Bool -> [(Bool,[Path])] -> [Path]
filterAndCombine b = combinePaths . map snd . filter ((==b) . fst)

allCombinedPaths :: [(Bool,[Path])] -> ([Path],[Path])
allCombinedPaths c = (tPaths, fPaths)
                   where tPaths = filterAndCombine True  c
                         fPaths = filterAndCombine False c

foldThTree :: (String, Bool, [Integer]) -> [(Bool, [Path])] ->  (Bool, [Path])
foldThTree (name, b, []) children = (,) b . foldl ((++)) [] . map snd $ children 
foldThTree (name, b, thVals) children = 
  let ths = map ((,) name) thVals
      (ctPaths,cfPaths) = allCombinedPaths children     
      tPaths  = map ((:) . head $ ths) ctPaths
      fPaths  = map ((:) . last $ ths) cfPaths
      tfPaths = (:) <$> (init . tail $ ths) <*> (ctPaths ++ cfPaths)
  in (,) b . foldl (++) [] $ [tPaths, tfPaths, fPaths]

-- Helper functions --
handleJsonFile :: String -> String -> (BS.ByteString -> a) -> IO a
handleJsonFile tmpName compCmd f = do
  silence $ callCommand compCmd 
  jsonDump <- BS.readFile tmpName
  let res = f jsonDump 
  removeFile tmpName
  return res

-- Sort paths by runtimes --
hackyRuntimeSort :: Runtimes -> Runtimes -> Ordering
hackyRuntimeSort a b =
  ra `compare` rb
  where ra = foldl (+) 0 . concat . map snd $ a
        rb = foldl (+) 0 . concat . map snd $ b

--[(String, [Integer])]

tune :: String -> String -> Integer -> (Runtimes -> Runtimes -> Ordering) -> IO [Path]
tune prog backend nRuns sortRuns = do
  comps <- getComps backend prog
  let thVals = allTHvals comps
  tree <- getStructure prog
  let paths = snd . foldTree foldThTree $ addTHvalsToTree thVals tree
  runs <- pathsToRuntimes nRuns prog paths
  let ordRuns = sortBy (sortRuns `on` snd) runs
  return . map fst $ ordRuns

-- Get Runtimes --
pathsToRuntimes :: Integer -> String -> [Path] -> IO [(Path,Runtimes)]
pathsToRuntimes nRuns prog paths = 
  mapM (\p -> liftM ((,) p) . benchRuntimes prog nRuns . createTuneStr $ p) paths

tuneThresSrt :: String -> Integer -> String
tuneThresSrt name val = name ++ '=':(show val) ++ ","

createTuneStr :: Path -> String
createTuneStr = concat . map (\(str,n) -> tuneThresSrt str n)

benchRuntimes ::  String -> Integer -> String -> IO Runtimes
benchRuntimes progName nRuns tuneCont = do
  let tuneFileName = (progName ++ ".tuning")
  writeFile tuneFileName tuneCont
  let tmpName = ".tmpJsonRuntimes.json"
  let cmd = "futhark bench --backend=opencl -r " ++ show nRuns ++ 
            " --exclude-case=notune --skip-compilation --json=" ++
            tmpName ++ " " ++ progName
  res <- handleJsonFile tmpName cmd (extractRuntimes . parseBenchJson)
  removeFile tuneFileName
  return res

extractRuntimes :: [(String, [(String, Dataset)])] -> Runtimes
extractRuntimes ((_,x):[]) = map (\(n,d) -> (n, runtimes d)) x
extractRuntimes _ =
  error "Json contains multiple programs, this is not currently supported."

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
