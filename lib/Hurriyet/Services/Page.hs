{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.Page where

import Data.Aeson
import GHC.Generics

{- TODO: Add relatedNews -}
data Page = Page
  { _id :: String
  , createdDate :: String
  , title :: String
  {- TODO: Check if pages have file field. If so, add it.
     Currently pages endpoint does not respond.
  -}
  , url :: String
  } deriving (Generic, Show)

instance FromJSON Page where
  parseJSON = withObject "page" $ \o -> do
    _id          <- o .: "Id"
    createdDate  <- o .: "CreatedDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    return Page {..}
