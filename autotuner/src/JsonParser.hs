{-# LANGUAGE OverloadedStrings #-}
module JsonParser (Dataset, err, runtimes, parseBenchJson) where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable
import qualified Data.HashMap.Strict as HM  
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as BS

data Dataset = Dataset { err::String, runtimes::[Integer] }
               deriving Show
instance FromJSON Dataset where
  parseJSON = withObject "dataset" $ \o -> do 
    err  <- o .: "stderr"
    runtimes <- o .: "runtimes" 
    return Dataset{err=err, runtimes=runtimes}

type DatasetName = String
newtype Program = Program [(DatasetName,Dataset)]
instance FromJSON Program where
  parseJSON = withObject "Program" $ \o -> do
    ds <- o .: "datasets"
    dss <- for (HM.toList ds) $ \(name, info) -> do
      pInfo <- parseJSON info
      return (T.unpack name, pInfo)
    return (Program dss)

type ProgramName = String      
newtype Programs = Programs [(ProgramName, Program)]
instance FromJSON Programs where
  parseJSON = withObject "Programs" $ \o -> do
    progs <- for (HM.toList o) $ \(name, prog) -> do
      pProg <- parseJSON prog
      return (T.unpack name, pProg)
    return $ Programs progs

unpackMaybePrograms :: Maybe Programs ->
                       [(ProgramName, [(DatasetName, Dataset)])]
unpackMaybePrograms (Just (Programs prog)) =
  map (\(n, (Program p)) -> (n,p)) prog

parseBenchJson :: BS.ByteString -> [(ProgramName, [(DatasetName, Dataset)])]
parseBenchJson = unpackMaybePrograms . decode
