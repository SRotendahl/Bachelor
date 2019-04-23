module Main where 

-- Our libaries --
import Regex
import Tree
import ParsingJson

-- Other libaries --
import System.Environment
import System.Process
import Data.List.Split --used for parsing name
import qualified Data.ByteString.Lazy as BS (readFile)
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

main = do
  args <- getArgs
  let pArgs = parseArgs args
  -- Get vals
  let compCmd = "futhark bench -r 1 --backend=opencl -p -L --json=jsonstuff " ++ getProgram pArgs
  callCommand compCmd 
  jsonDump <- BS.readFile "jsonstuff"
  let (Just (DatasetS test)) = valVal jsonDump
  -- Get structure
  let name1 = getName $ getProgram pArgs
  let name = "./" ++ name1
  sizes <- readProcess name ["--print-sizes"] ""
  let thresh = getTresh sizes
  putStrLn . printTree . (fmap show) $ buildTree (head test) thresh
   
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
