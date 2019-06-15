{-# LANGUAGE OverloadedStrings #-}
module Autotuning (main) where

import Data.Ord
import Data.Function
import Data.Tree
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import Text.Regex
import Text.Regex.Base

import System.IO
import System.IO.Silently
import System.Process
import System.Directory

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.HashMap.Strict as HM  
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- Tuning function and its helper --
type Path = [(String, Integer)]
type Runtimes = [(String, [Integer])]

-- Sort paths by runtimes --
runtimeSortTotal :: Runtimes -> Runtimes -> Ordering
runtimeSortTotal len a b =
  ra `compare` rb
  where ra = foldl (+) 0 . concat . map snd $ a
        rb = foldl (+) 0 . concat . map snd $ b

tune :: String -> String -> String -> Integer -> IO [Path]
tune execute prog backend nRuns = do
  comps <- getComps execute backend prog 
  let thVals = allTHvals comps
  tree <- getStructure prog
  let paths = snd . foldTree foldThTree $ addTHvalsToTree thVals tree
  putStrLn $ "Number of paths: " ++ (show . length $ paths)
  runs <- pathsToRuntimes nRuns prog execute paths
  let ordRuns = sortBy (hackyRuntimeSort `on` snd) runs
  return . map fst $ ordRuns 

-- Calculating Threshold values --
combineTHvals :: [(String, Integer)] -> [(String, [Integer])]
combineTHvals = map (\l -> (fst . head $ l, nub $ map snd l))
                . groupBy ((==) `on` fst) . sortBy (comparing fst)

addFalseVal :: [Integer] -> [Integer]
addFalseVal xs = xs ++ (((+1) . last $ xs):[])

allTHvals :: [(String, [(String, Integer)])]
          -> [(String, [Integer])]
allTHvals = map (\th -> (fst th, addFalseVal . sort . snd $ th))
            . combineTHvals . concat . map snd

addTHvalsToTree :: [(String, [Integer])] -> (Tree (String, Bool)) -> (Tree (String, Bool, [Integer]))
addTHvalsToTree thVals = fmap (\(name, bool) -> (name, bool, fromMaybe []$ lookup name thVals))

-- Fold Tree to list of paths --
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

-- Json functions --
data Dataset = Dataset { err::String, runtimes::[Integer] }
               deriving Show
instance FromJSON Dataset where
  parseJSON = withObject "dataset" $ \o -> do 
    err  <- o .: "stderr"
    runtimes <- o .: "runtimes" 
    return Dataset{err=err, runtimes=runtimes}

type DatasetName = String
newtype Program = Program [(DatasetName,Dataset)]
instance FromJSON Program where
  parseJSON = withObject "Program" $ \o -> do
    ds <- o .: "datasets"
    dss <- for (HM.toList ds) $ \(name, info) -> do
      pInfo <- parseJSON info
      return (T.unpack name, pInfo)
    return (Program dss)

type ProgramName = String      
newtype Programs = Programs [(ProgramName, Program)]
instance FromJSON Programs where
  parseJSON = withObject "Programs" $ \o -> do
    progs <- for (HM.toList o) $ \(name, prog) -> do
      pProg <- parseJSON prog
      return (T.unpack name, pProg)
    return $ Programs progs

unpackMaybePrograms :: Maybe Programs ->
                       [(ProgramName, [(DatasetName, Dataset)])]
unpackMaybePrograms (Just (Programs prog)) =
  map (\(n, (Program p)) -> (n,p)) prog

parseBenchJson :: BS.ByteString -> [(ProgramName, [(DatasetName, Dataset)])]
parseBenchJson = unpackMaybePrograms . decodeStrict'

handleJsonFile :: String -> String -> (BS.ByteString -> a) -> IO a
handleJsonFile tmpName compCmd f = do
  silence $ callCommand compCmd 
  jsonDump <- BS.readFile tmpName
  let res = f jsonDump 
  removeFile tmpName
  return res

-- Get Runtimes --
pathsToRuntimes :: Integer -> String -> String -> [Path] -> IO [(Path,Runtimes)]
pathsToRuntimes nRuns prog execute paths = do
  mapM (pathToRuntime $ benchRuntimes execute prog nRuns) $ zip paths [1..]

pathToRuntime :: (String -> IO Runtimes) -> (Path,Integer) -> IO (Path,Runtimes)
pathToRuntime runF (p,n) = do 
  let pStr = createTuneStr tuneThresSrt p
  if n `mod` 10 == 1 then putStrLn $ "Path nr. " ++ (show n) else return ()
  runT <- runF pStr
  return (p,runT)

tuneThresSrt :: String -> Integer -> String
tuneThresSrt name val = "-p --size=" ++ name ++ '=':(show val) ++ " "

createTuneStr :: (String -> Integer -> String) -> Path -> String
createTuneStr f = concat . map (\(str,n) -> f str n)

benchRuntimes :: String ->  String -> Integer -> String -> IO Runtimes
benchRuntimes execute progName nRuns tuneCont = do
  let tmpName = ".tmpJsonRuntimes.json"
  let cmd = execute ++ " bench --backend=opencl -r " ++ show nRuns ++ 
            " --exclude-case=notune --no-tuning --skip-compilation --json=" ++
            tmpName ++ " " ++ tuneCont ++ progName
  res <- handleJsonFile tmpName cmd (extractRuntimes . parseBenchJson)
  return res

extractRuntimes :: [(String, [(String, Dataset)])] -> Runtimes
extractRuntimes ((_,x):[]) = map (\(n,d) -> (n, runtimes d)) x
extractRuntimes [] =
  error "Json contains no programs."
extractRuntimes _ =
  error "Json contains multiple programs, this is not supported."

-- Get comparison values --
extractComps :: [(String, [(String, Dataset)])] -> [(String, [(String, Integer)])]
extractComps ((_,x):[]) = map (\(n,d) -> (n, nub . getComparison . err $ d)) x
extractComps [] =
  error "Json contains no programs."
extractComps _ =
  error "Json contains multiple programs, this is not supported."

getComps :: String -> String -> String -> IO [(String, [(String, Integer)])]
getComps execute backend progName = do
  let tmpName = ".tmpBenchOutput.json"
  let cmd = execute ++ " bench --backend=opencl -p -L  " ++ progName ++ 
            " --exclude-case=notune --json="
            ++ tmpName
  handleJsonFile tmpName cmd $ extractComps . parseBenchJson 

compToTuple :: [String] -> (String,Integer)
compToTuple (thresh:val:[]) = (thresh, read val) 

getComparison :: String -> [(String,Integer)] 
getComparison comp = 
  let regex = mkRegex "Compared ([^ ]+) <= (-?[0-9]+)"
      splitLine = splitOn "\n" comp 
      matches = catMaybes $ map (matchRegex regex) splitLine
  in  map compToTuple matches

-- Get threshold structure --
getStructure :: String -> IO (Tree (String,Bool))
getStructure prog = do
  sizes <- readProcess (dropExtension prog) ["--print-sizes"] ""
  return . buildTree . getTresh $ sizes

cleanGroups :: [String] -> (String,[(String,Bool)])
cleanGroups (grp1:grp2:[]) =
  (grp1, 
    filter (not . (=="") . fst) .
    map (\x -> (x!!1, not . (=="!") . (!!0) $ x)) .
    catMaybes .
    map (matchRegex $ mkRegex "(!?)(.*)") .
    splitOn " "
    $ grp2
  )

removeAll :: (Eq a) => a -> [[a]] -> [[a]]
removeAll = map . filter . (/=)

getTresh :: String -> [(String,[(String,Bool)])]
getTresh sizeOutput = -- Prob needs to be split up a bit
  let regex = mkRegex "(.*)\\ \\(threshold\\ \\((.*)\\)\\)"       
      outputLines = lines sizeOutput
      matches = catMaybes $ map (matchRegex regex) outputLines
      getGroups = cleanGroups 
  in  map getGroups matches

getNextTHs :: [(String,Bool)] -> [((String,Bool),[String])] -> [[(String,Bool)]]
getNextTHs prevThs =
  map ((:prevThs) . fst) .
  filter (((==) $ sort . map fst $ prevThs) . sort . snd)

toTree :: [((String,Bool),[String])] -> [(String,Bool)] -> ((String,Bool), [[(String,Bool)]])
-- nullRoot is simply to make sure that if no thresholds are found a tree is 
-- still made
toTree ths [] = 
  (("nullRoot",False), roots) 
  where roots = map ((:[]) . fst) $ filter (null . snd) ths 
toTree ths prevThs =
  (currThs, nextThs)
  where currThs  = head prevThs 
        nextThs = getNextTHs prevThs ths

checkTHs :: [(String, [(String, Bool)])] -> [((String,Bool), [String])]
checkTHs ths = 
  let thBls = map ((\(a,b) -> (head a,b)) . unzip)
            . groupBy ((==) `on` fst) . sort . concat . map snd $ ths 
      pred = and . map ((==) 2 . length . snd) $ thBls 
      res = map (\(n,ts) -> ((,) n . head . fromMaybe [False] . lookup n $ thBls, map fst ts)) ths
  in  if pred then error "Multiple parents not supported" else res 
      
buildTree :: [(String, [(String,Bool)])] -> (Tree (String, Bool))
buildTree threshs = 
  unfoldTree (toTree $ checkTHs threshs) [] 
                    
printTree :: (Show a) => Tree a -> String
printTree = drawTree . fmap show 

---------------- CLI ------------------
main :: String -> [String] -> IO ()
main = mainWithOptions initialAutotuneOptions commandLineOptions
  "options...       programs..." $ \progs config -> Just $ runAutotuner config progs

runAutotuner :: AutotuneOptions -> [FilePath] -> IO ()
runAutotuner opts [x] = do
  res <- head $ tune (optFuthark opts) (optBackend opts) (optRuns opts) x
  let outStr = outThresholds (fst res) (snd res)
  case optRes opts of 
    Nothing -> return ()
    Just file -> writeFile file outStr
  putStrLn "Tuning file Content:\n" ++ outStr

runAutotuner opts xs = error "Mutiple programs are not currently supported"

outThresholds :: String -> Integer -> String
outThresholds name val = name ++ '=':(show val) ++ ","

data AutotuneOptions = AutotuneOptions
                    { optBackend :: String
                    , optFuthark :: String
                    , optRuns :: Int
                    , optRes :: Maybe FilePath
                    , optEntryPoint :: Maybe String
                    }

initialAutotuneOptions :: AutotuneOptions
initialAutotuneOptions = AutotuneOptions "opencl" "futhark" 10 Nothing Nothing

commandLineOptions :: [FunOptDescr BenchOptions]
commandLineOptions = [
    Option "r" ["runs"]
    (ReqArg (\n ->
              case reads n of
                [(n', "")] | n' >= 0 ->
                  Right $ \config ->
                  config { optRuns = n'
                         }
                _ ->
                  Left $ error $ "'" ++ n ++ "' is not a non-negative integer.")
     "RUNS")
    "Run each test case this many times."
  , Option [] ["backend"]
    (ReqArg (\backend -> Right $ \config -> config { optBackend = backend })
     "PROGRAM")
    "The compiler used (defaults to 'futhark-opencl')."
  , Option [] ["futhark"]
    (ReqArg (\prog -> Right $ \config -> config { optFuthark = prog })
     "PROGRAM")
    "The binary used for operations (defaults to 'futhark')."
   , Option [] ["result"]
     (ReqArg (\file ->
                Right $ \config -> config { optRes = Just file})
     "FILE")
     "Where to save the resulting threshold values"
   , Option "e" ["entry-point"]
     (ReqArg (\s -> Right $ \config ->
                 config { optEntryPoint = Just s })
       "NAME")
     "Only run this entry point."
   ]
   where max_timeout :: Int
         max_timeout = maxBound `div` 1000000
