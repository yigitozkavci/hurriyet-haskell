{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.Article where

import Data.Aeson
import GHC.Generics
import Hurriyet.Services.File

data Article = Article
  { id'          :: String
  , contentType  :: String
  , createdDate  :: String
  , description  :: String
  {- Hurriyet API does not include modifiedDate in their `show` response for articles yet.
     Tracking issue: https://github.com/hurriyet/developers.hurriyet.com.tr/issues/27
  -}
  -- , modifiedDate :: String
  , path         :: String
  , files        :: [File]
  , startDate    :: String
  , title        :: String
  , url          :: String
  } deriving (Generic, Show, Eq)

instance FromJSON Article where
  parseJSON = withObject "article" $ \o -> do
    id'          <- o .: "Id"
    contentType  <- o .: "ContentType"
    createdDate  <- o .: "CreatedDate"
    description  <- o .: "Description"
    files        <- o .: "Files"
    -- modifiedDate <- o .: "ModifiedDate"
    path         <- o .: "Path"
    startDate    <- o .: "StartDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    return Article {..}

instance ToJSON Article
