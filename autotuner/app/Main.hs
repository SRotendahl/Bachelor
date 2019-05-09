module Main where 

-- Our libaries --
import Regex
import Tree
import ParsingJson
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


getComparisons :: String -> String -> Bool -> IO [(String, [(String,Int)])]
getComparisons backend progName isCompiled = do
  let jsonName = ".tmpBenchOutput.json"
  let compCmd = buildBenchCmd backend progName jsonName isCompiled
  callCommand compCmd 
  jsonDump <- BS.readFile jsonName
  removeFile jsonName
  return . valVal $ jsonDump

getStructure :: String -> IO [(String,[String])]
getStructure prog = do
  let name = "./" ++ (getName prog)
  sizes <- readProcess name ["--print-sizes"] ""
  return . getTresh $ sizes

main = do
  args <- getArgs
  let pArgs = parseArgs args
  compiled <- doesFileExist $ head (splitOn "." (getProgram pArgs)) 
  comps <- getComparisons (getBackend pArgs) (getProgram pArgs) compiled
  thresh <- getStructure $ getProgram pArgs
  let tree = buildTree thresh 
  let exe = getExecutions tree
  runTimes <- mapM (\ex -> tuneProgram (snd (head comps)) ex "tuning" (getProgram pArgs)) exe
  print "----------------- runing times ---------------------"
  print  runTimes
  print "----------------- executions -----------------------"
  mapM_ print exe
    {-
  print "----------------- comparisions ---------------------"
  print $ removeDup (snd (head comps)) 
  print "----------------- tune parameters ------------------"
  print tunePara
  -}
  putStrLn . printTree . (fmap show) $ tree
  --print $ getExecutions tree 
{-
  args <- getArgs
  let pArgs = parseArgs args
  let command = "futhark " ++ getBackend pArgs ++ " " ++ getProgram pArgs
  putStrLn $ "Compiling with: " ++ command ++ "\n"
  callCommand command
  let name1 = getName $ getProgram pArgs
  let name = "./" ++ name1
  sizes <- readProcess name ["--print-sizes"] ""
  let thresh = getTresh sizes
  --TEST
  let compCmd = "futhark bench -r 1 --backend=opencl -p -L --json=tmp " ++ getProgram pArgs
  callCommand compCmd
  compData <- readFile "tmp"
  let comp = getComparison compData
  print comp
  --TEST
  putStrLn . printTree . buildTree $ thresh
-}
-----------------------
