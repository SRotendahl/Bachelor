import System.Environment
import Data.Array
import Text.Regex
import System.Process
import Text.Regex.Base
import Data.List.Split
import Data.List
import Data.Function

getTresh :: [String] -> [[[Char]]]
getTresh matches =
  let mkPattern   = mkRegex "threshold"
      f = filter (\x -> matchTest mkPattern x) matches
      split = map (\x -> splitOn " " x) f
  in map (\xx -> filter (\x -> not $ matchTest mkPattern x) xx) split

{-
sortList :: [[[Char]]] -> [[[Char]]]
sortList l =
  groupBy ((==) `on` length) $ sortBy (compare `on` length) l
-}
