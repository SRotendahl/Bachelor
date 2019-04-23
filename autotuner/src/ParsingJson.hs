{-# LANGUAGE OverloadedStrings #-}
module ParsingJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad
import qualified Data.ByteString.Lazy as BS

import Regex --(getComparion)

newtype DatasetS = DatasetS [Dataset] deriving Show
instance FromJSON DatasetS where
  parseJSON = parseVal 

newtype Dataset = Dataset (String, [(String, Int)]) deriving Show

parseVal = withObject "program" $ \o -> 
  liftM (DatasetS . head) $       -- Only be one element in the list
  for (HM.toList o) $ \(progName, mo) -> do
    dataSets <- withObject "datasets" (\mobj -> mobj .: "datasets") mo
    parseValHelp dataSets

parseValHelp :: Value -> Parser [Dataset]
parseValHelp = withObject "data" $ \o ->
  for (HM.toList o) $ \(name, ds) -> do
    stderr <- withObject "stderr" (\dso -> dso .: "stderr") ds
    let regStderr = getComparison stderr
    return $ Dataset (T.unpack name, regStderr)

valVal :: BS.ByteString -> Maybe DatasetS
valVal = decode  --TODO change name
