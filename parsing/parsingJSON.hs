{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Text.Regex
import Text.Regex.Base
import Data.Maybe
import Data.List.Split
import Data.List
newtype Dataset = Dataset (String, [(String, Int)])
{-
parseJSON progName = withObject "JSON Dump" $ \o -> do
	prog <- o .: progName
	sets <- o .: "datasets"
-}
--instance FromJSON Dataset where
--parseDatasets :: Value -> Parser [Dataset]
parseDatasets = withObject "Dataset" $ \o ->
  for (HM.toList o) $ \(name, dso) -> do
    return $ T.unpack name
{-
		err <- T.unpack $ dso .: "stderr" --TODO better name for err
		let comps = findComps err
		let uname = T.unpack name 
		return $ Dataset (uname, comps)
-}

findComps comp =
  let regex = mkRegex "Compared ([^ ]+) <= (-?[0-9]+)"
      splitLine = splitOn "\\n" comp
      matches = catMaybes $ map (matchRegex regex) splitLine
  in  map compToTuple matches

compToTuple :: [String] -> (String,Int)
compToTuple (thresh:val:[]) = (thresh, read val)

