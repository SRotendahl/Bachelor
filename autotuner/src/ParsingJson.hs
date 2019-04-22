{-# LANGUAGE OverloadedStrings #-}
module ParsingJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

newtype Dataset = Dataset (String, String) deriving Show

parseVal :: Value -> Parser [Dataset]
parseVal = withObject "data" $ \o ->
  for (HM.toList o) $ \(name, ds) -> do
    stderr <- withObject "stderr" (\dso -> dso .: "stderr") ds
    --Regex here on stderr
    return $ Dataset (T.unpack name, stderr)

valVal val = parseMaybe parseVal val --TODO change name
