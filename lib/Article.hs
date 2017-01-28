{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Article where

import Data.Aeson
import GHC.Generics

{- TODO: Add files -}
data Article = Article
  { _id          :: String
  , contentType  :: String
  , createdDate  :: String
  , description  :: String
  , modifiedDate :: String
  , path         :: String
  , startDate    :: String
  , title        :: String
  , url          :: String
  } deriving (Generic, Show)

instance FromJSON Article where
  parseJSON = withObject "article" $ \o -> do
    _id          <- o .: "Id"
    contentType  <- o .: "ContentType"
    createdDate  <- o .: "CreatedDate"
    description  <- o .: "Description"
    modifiedDate <- o .: "ModifiedDate"
    path         <- o .: "Path"
    startDate    <- o .: "StartDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    return Article {..}

instance ToJSON Article

