{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.NewsPhotoGallery where

import Data.Aeson
import GHC.Generics
import Hurriyet.Services.File
import Hurriyet.Services.Service

data NewsPhotoGallery = NewsPhotoGallery
  { _id          :: String
  , contentType  :: String
  , createdDate  :: String
  , description  :: String
  , files        :: [File]
  , modifiedDate :: Maybe String
  , path         :: String
  , startDate    :: String
  , title        :: String
  , url          :: String
  } deriving (Generic, Show, Eq)

instance FromJSON NewsPhotoGallery where
  parseJSON = withObject "news_photo_gallery" $ \o -> do
    _id          <- o .: "Id"
    contentType  <- o .: "ContentType"
    createdDate  <- o .: "CreatedDate"
    description  <- o .: "Description"
    files        <- o .: "Files"
    modifiedDate <- o .:? "ModifiedDate"
    path         <- o .: "Path"
    startDate    <- o .: "StartDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    return NewsPhotoGallery {..}

instance ToJSON NewsPhotoGallery

instance Service NewsPhotoGallery where
  endpoint _ = "newsphotogalleries"
