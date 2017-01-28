{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Writer where

import Data.Aeson
import GHC.Generics

{- TODO: Add files -}
data Writer = Writer
  { _id         :: String
  , fullName    :: String
  , contentType :: String
  , createdDate :: String
  , path        :: String
  , url         :: String
  } deriving (Generic, Show)

instance FromJSON Writer where
  parseJSON = withObject "writer" $ \o -> do
    _id         <- o .: "Id"
    fullName    <- o .: "Fullname"
    contentType <- o .: "ContentType"
    createdDate <- o .: "CreatedDate"
    path        <- o .: "Path"
    url         <- o .: "Url"
    return Writer {..}

instance ToJSON Writer


