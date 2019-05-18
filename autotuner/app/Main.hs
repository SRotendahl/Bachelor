module Main where 

-- Our libaries --
import Regex
import Tree
import JsonParser
import Tuning

-- Tmp libaries --
--import System.IO.Silently
import Data.Maybe
import Data.List
--import Text.Regex
--import Text.Regex.Base

-- Other libaries --
import System.Environment
import System.Process
import Data.List.Split --used for parsing name
import qualified Data.ByteString.Lazy as BS
import System.Directory
--import qualified Data.ByteString.Lazy as BS (readFile)
----- Parse args -------
-- TODO: move to seperate module
data Flag = None --extend if we create flags
type Program = String
type Backend = String
type Args = (Backend, Program, [Flag])

getBackend :: Args -> Backend
getBackend (b,_,_) = b
getProgram :: Args -> Program
getProgram (_,p,_) = p
getFlags :: Args -> [Flag]
getFlags (_,_,f) = f

parseArgs :: [String] -> Args
parseArgs (backend:program:flags) =
  (backend, program, parseFlags flags)
parseArgs _ = error "TODO: Add guide to use here"

parseFlags :: [String] -> [Flag]
parseFlags [] = [None]
parseFlags _ = error "TODO: Add guide to use here"

getName :: String -> String
getName name =
  head $ splitOn "." name
------------------------

-------- Main ----------
buildBenchCmd backend progName tmpName isCompiled = 
  if isCompiled then 
    "futhark bench  --skip-compilation -r 1 --backend=" ++ backend ++ 
    " -p -L --json=" ++ tmpName ++ " " ++ progName
  else "futhark bench -r 1 --backend=" ++ backend ++  " -p -L --json=" 
    ++ tmpName ++ " " ++ progName

handleJsonFile :: String -> String -> (BS.ByteString -> a) -> IO a
handleJsonFile tmpName compCmd f = do
  callCommand compCmd 
  jsonDump <- BS.readFile tmpName
  let res = f jsonDump 
  removeFile tmpName
  return res

getJsonData :: String -> String -> Bool -> IO [(String,Dataset)]
getJsonData backend progName isCompiled = do
  let jsonName = ".tmpBenchOutput.json"
  let cmd = buildBenchCmd backend progName jsonName isCompiled
  jsonData <- handleJsonFile jsonName cmd parseBenchJson 
  return . snd . head $ jsonData 
  -- (snd . head) because there is only one program

getComparisons :: (String, Dataset) -> (String,[(String, Int)])
getComparisons (name, dset) = (name, getComparison . err $ dset)
  
getStructure :: String -> IO [(String,[String])]
getStructure prog = do
  let name = "./" ++ (getName prog)
  sizes <- readProcess name ["--print-sizes"] ""
  return . getTresh $ sizes

main = do
  args <- getArgs
  let pArgs = parseArgs args
  compiled <- doesFileExist $ head (splitOn "." (getProgram pArgs)) 

  compsDSet <- getJsonData (getBackend pArgs) (getProgram pArgs) compiled
  let comps = map getComparisons compsDSet 

  thresh <- getStructure $ getProgram pArgs

  let tree = buildTree thresh 
  let exe = getExecutions tree

  runTimes <- mapM (\ex -> tuneProgram (snd (head comps)) ex "tuning" (getProgram pArgs)) exe
  let strs = map (\ex -> createTuneFile (snd (head comps)) ex) exe
  print "----------------- runing times ---------------------"
  mapM_ print $ zip runTimes strs
  print "----------------- executions -----------------------"
  mapM_ print exe
  print "----------------- comparisions ---------------------"
  print $ snd (head comps) 
  putStrLn . printTree . (fmap show) $ tree
-----------------------
