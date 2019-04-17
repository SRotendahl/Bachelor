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
- }

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
      matches = map (matchRegexAll regex) outputLines
      getGroups = cleanGroups . removeAll ' ' . (\(_,_,_,x) -> x)
  in  catMaybes $ map (liftM getGroups) matches -- name

------------------
------ tree ------
{-
buildTree :: [(String,[String])] -> Tree 
buildTree treshs =
    ps = all elements where snd is []
    newTresh = tresh / ps
    map over ps with as p: --Will be empty if non are found.
        remove p from snd in newTresh
        subtree = buildTree newTresh
        set p as parent of subtree
-}

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
  mapM_ print thresh
{-
tree :: Tree String
tree = Node "hello" [ Node "foo" []
                     , Node "bars" [ Node "oi!" []
                                   , Node "baz" [ Node "a" [ Node "b" []
                                                           , Node "c" []]
                                                , Node "d" [ Node "e" []]]]
                     , Node "foobar" []]

-}
