import System.Environment
import Data.Array
import Text.Regex
import System.Process
import Text.Regex.Base
import Data.List
import Data.List.Split
import Data.Tree
--import Data.Tree.Pretty
import Data.Typeable
import Control.Monad
import Data.Maybe

{- NOTES
 - Alias (String,[String]) to a better name
 -
-}

------------ regex --------------
cleanGroups :: [String] -> (String,[String])
cleanGroups (grp1:grp2:[]) =
  (grp1 ,tail (splitOn "!" grp2))

removeAll :: (Eq a) => a -> [[a]] -> [[a]]
removeAll = map . filter . (/=)

getTresh :: String -> [(String,[String])]
getTresh sizeOutput = -- Prob needs to be split up a bit
  let regex = mkRegex "(.*)\\ \\(threshold\\ \\((.*)\\)\\)"       
      outputLines = lines sizeOutput
      matches = catMaybes $ map (matchRegexAll regex) outputLines
      getGroups = cleanGroups . removeAll ' ' . (\(_,_,_,x) -> x)
  in  map getGroups matches 

------------------
------ tree ------

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

------------------------

----- Parse args -------
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
------------------------

-------- Main ----------

getName :: String -> String
getName name =
  head $ splitOn "." name

main = do
  args <- getArgs
  let pArgs = parseArgs args
  let command = "futhark " ++ getBackend pArgs ++ " " ++ getProgram pArgs
  putStrLn $ "Compiling with: " ++ command ++ "\n"
  callCommand command
  let name1 = getName $ getProgram pArgs
  let name = "./" ++ name1
  sizes <- readProcess name ["--print-sizes"] ""
  let thresh = getTresh sizes
  let tree = unfoldTree (toTree thresh) []
  putStrLn $ drawTree tree
-------------------------
