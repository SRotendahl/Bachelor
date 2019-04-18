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
  (grp1 ,tail (splitOn "!main." grp2))

removeAll :: (Eq a) => a -> [[a]] -> [[a]]
removeAll = map . filter . (/=)

getTresh :: String -> [(String,[String])]
getTresh sizeOutput = -- Prob needs to be split up a bit
  let regex = mkRegex "main\\.(.*)\\ \\(threshold\\ \\((.*)\\)\\)"       
      outputLines = lines sizeOutput
      matches = catMaybes $ map (matchRegexAll regex) outputLines
      getGroups = cleanGroups . removeAll ' ' . (\(_,_,_,x) -> x)
  in  map getGroups matches 

------------------
------ tree ------


treeLevel :: [(String,[String])] -> [String] -> (String, [[String]])
treeLevel thress [] = 
  ("nullRoot", roots)
  where roots = map ((:[]) . fst) $ filter (null . snd) thress 
treeLevel thress prevThress =
  (currThres, nextThress)
  where currThres  = head prevThress 
        nextThress = map ((:prevThress) . fst) $ -- Should be it's own function
                     filter (((==) $ sort prevThress) . sort . snd) thress

------------------

getName :: String -> String
getName name =
  head $ splitOn "." name

main = do
  args <- getArgs
  putStrLn $ "futhark " ++ args !! 0 ++ " " ++ args !! 1
  callCommand $ "futhark " ++ args !! 0 ++ " " ++ args !! 1
  let name1 = getName $ args !! 1
  let name = "./" ++ name1
  sizes <- readProcess name ["--print-sizes"] ""
  let thresh = getTresh sizes
  let tree = unfoldTree (treeLevel thresh) []
  putStrLn $ drawTree tree
