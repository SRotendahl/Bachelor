module Regex (getTresh) where

--import Data.Array
import Text.Regex
import Text.Regex.Base
import Data.List.Split
import Data.Maybe

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
