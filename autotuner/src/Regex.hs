module Regex (getTresh,getComparison) where

--import Data.Array
import Text.Regex
import Text.Regex.Base
import Data.List.Split
import Data.List
import Data.Maybe

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

compToTuple :: [String] -> (String,Integer)
compToTuple (thresh:val:[]) = (thresh, read val) 

getComparison :: String -> [(String,Integer)] -- TODO make better alias
getComparison comp = 
  let regex = mkRegex "Compared ([^ ]+) <= (-?[0-9]+)"
      splitLine = splitOn "\n" comp 
      matches = catMaybes $ map (matchRegex regex) splitLine
  in  map compToTuple matches
      
