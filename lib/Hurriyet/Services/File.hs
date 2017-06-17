{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.File where

import Data.Aeson
import GHC.Generics

data File = File
  { fileUrl  :: String
  , metadata :: Metadata
  } deriving(Generic, Show, Eq)

instance FromJSON File where
  parseJSON = withObject "file" $ \o -> do
    fileUrl  <- o .: "FileUrl"
    metadata <- o .: "Metadata"
    return File{..}

instance ToJSON File

newtype Metadata = Metadata
  { title       :: String
  -- , description :: String -- This field does not exist on article show page. Tracking issue: https://github.com/hurriyet/developers.hurriyet.com.tr/issues/28
  } deriving(Generic, Show, Eq)

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \o -> do
    title       <- o .: "Title"
    -- description <- o .: "Description"
    return Metadata{..}

instance ToJSON Metadata
