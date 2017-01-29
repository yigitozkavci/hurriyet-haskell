{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Writer where

import Data.Aeson
import GHC.Generics
import File

data Writer = Writer
  { _id         :: String
  , fullName    :: String
  , contentType :: String
  , createdDate :: String
  , files       :: [File]
  , path        :: String
  , url         :: String
  } deriving (Generic, Show)

instance FromJSON Writer where
  parseJSON = withObject "writer" $ \o -> do
    _id         <- o .: "Id"
    fullName    <- o .: "Fullname"
    contentType <- o .: "ContentType"
    createdDate <- o .: "CreatedDate"
    files       <- o .: "Files"
    path        <- o .: "Path"
    url         <- o .: "Url"
    return Writer {..}

instance ToJSON Writer


