{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.Column where

import Data.Aeson
import GHC.Generics
import Hurriyet.Services.File
import Hurriyet.Services.Service

data Column = Column
  { _id          :: String
  , fullName     :: String
  , contentType  :: String
  , createdDate  :: String
  , description  :: String
  , files        :: [File]
  , path         :: String
  , startDate    :: String
  , title        :: String
  , url          :: String
  , writerId     :: Maybe String
  } deriving (Generic, Show, Eq)

instance FromJSON Column where
  parseJSON = withObject "column" $ \o -> do
    _id          <- o .: "Id"
    fullName     <- o .: "Fullname"
    contentType  <- o .: "ContentType"
    createdDate  <- o .: "CreatedDate"
    description  <- o .: "Description"
    files        <- o .: "Files"
    path         <- o .: "Path"
    startDate    <- o .: "StartDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    writerId     <- o .:? "WriterId"
    return Column {..}

instance ToJSON Column

instance Service Column where
  endpoint _ = "columns"
