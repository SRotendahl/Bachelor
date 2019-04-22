{-# LANGUAGE OverloadedStrings #-}
module ParsingJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Regex --(getComparion)
newtype Dataset = Dataset (String, [(String, Int)]) deriving Show

parseVal progName = withObject "program" $ \o -> do
  name <- o .:  (T.pack progName)
  dataSets <- name .: "datasets"
  parseValHelp dataSets

parseValHelp :: Value -> Parser [Dataset]
parseValHelp = withObject "data" $ \o ->
  for (HM.toList o) $ \(name, ds) -> do
    stderr <- withObject "stderr" (\dso -> dso .: "stderr") ds
    let regStderr = getComparison stderr
    return $ Dataset (T.unpack name, regStderr)

valVal progName val = parseMaybe (parseVal progName) val --TODO change name
