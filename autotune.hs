import System.Environment
import Data.Array
import Text.Regex
import System.Process
import Text.Regex.Base
import Data.List
import Data.List.Split
import Data.Tree
import Data.Tree.Pretty
import Data.Typeable

getTresh :: String -> [[String]]
getTresh matches =
  let mkPattern   = mkRegex "threshold"
      matchesList = lines matches
      f = filter (\x -> matchTest mkPattern x) matchesList
      split = map (\x -> splitOn " " x) f
      f1 = map (\xx -> filter (\x -> not $ matchTest mkPattern x) xx) split
  in reverse . map(\z -> reverse z) . sort $ map (\x -> sort x) f1

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
