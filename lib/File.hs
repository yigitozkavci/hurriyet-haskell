{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module File where

import Data.Aeson
import GHC.Generics

data File = File
  { fileUrl  :: String
  , metadata :: Metadata
  } deriving(Generic, Show)

instance FromJSON File where
  parseJSON = withObject "file" $ \o -> do
    fileUrl  <- o .: "FileUrl"
    metadata <- o .: "Metadata"
    return File{..}

instance ToJSON File

data Metadata = Metadata
  { title       :: String
  , description :: String
  } deriving(Generic, Show)

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \o -> do
    title       <- o .: "Title"
    description <- o .: "Description"
    return Metadata{..}

instance ToJSON Metadata
