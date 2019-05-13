{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS
import Text.Regex
import Text.Regex.Base
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Traversable
import qualified Data.Text as T

cleanGroups :: [String] -> (String,[String])
cleanGroups (grp1:grp2:[]) =
  (grp1 , tail (splitOn "!" grp2))

removeAll :: (Eq a) => a -> [[a]] -> [[a]]
removeAll = map . filter . (/=)

getTresh :: String -> [(String,[String])]
getTresh sizeOutput = -- Prob needs to be split up a bit
  let regex = mkRegex "(.*)\\ \\(threshold\\ \\((.*)\\)\\)"       
      outputLines = lines sizeOutput
      matches = catMaybes $ map (matchRegex regex) outputLines
      getGroups = cleanGroups . removeAll ' ' 
  in  map getGroups matches

compToTuple :: [String] -> (String,Integer)
compToTuple (thresh:val:[]) = (thresh, read val) 

getComparison :: String -> [(String,Integer)] -- TODO make better alias
getComparison comp = 
  let regex = mkRegex "Compared ([^ ]+) <= (-?[0-9]+)"
      splitLine = splitOn "\n" comp 
      matches = catMaybes $ map (matchRegex regex) splitLine
  in  map compToTuple matches

data DatasetJSON = DatasetJSON { err::String,
                         runtimes::[Integer] }
               deriving Show

instance FromJSON DatasetJSON where
  parseJSON = withObject "dataset" $ \o -> do
    err  <- o .: "stderr"
    runtimes <- o .: "runtimes"
    return DatasetJSON{err=err, runtimes=runtimes}

newtype Program = Program [(String,DatasetJSON)]
        deriving Show

instance FromJSON Program where
  parseJSON = withObject "Program" $ \o -> do
    ds <- o .: "datasets"
    dss <- for (HM.toList ds) $ \(name, info) -> do
      pInfo <- parseJSON info
      return (T.unpack name, pInfo)
    return (Program dss)


newtype Programs = Programs [(String, Program)] deriving Show
instance FromJSON Programs where
  parseJSON = withObject "Programs" $ \o -> do
    progs <- for (HM.toList o) $ \(name, prog) -> do
      pProg <- parseJSON prog
      return (T.unpack name, pProg)
    return $ Programs progs

unpackMaybePrograms :: Maybe Programs -> [(String, [(String, DatasetJSON)])]
unpackMaybePrograms (Just (Programs prog)) =
  map (\(n, (Program p)) -> (n,p)) prog

data Dataset = Dataset {name :: String, comps :: [(String, Integer)], runTime :: [Integer]} deriving Show

toDatasets programs = --TODO better name
  map (\prog -> (fst prog, 
       map (\datasets -> Dataset {name = fst datasets, comps = getComparison (err (snd datasets)), runTime = runtimes (snd datasets) }) 
                 (snd prog)
                )       ) programs  


main :: IO ()
main = do
  jsonFile <- BS.readFile "toBeParsed.json"
  let test2 = unpackMaybePrograms . decode $ jsonFile
  mapM_ print (toDatasets test2)
  --print $ getComparison (err . snd . head . snd . head $ test2)
