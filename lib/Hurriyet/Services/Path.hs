{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Hurriyet.Services.Path where

import Data.Aeson
import GHC.Generics
import Hurriyet.Services.Service

data Path = Path
  { _id          :: String
  , path         :: String
  , title        :: String
  } deriving (Generic, Show, Eq)

instance FromJSON Path where
  parseJSON = withObject "path" $ \o -> do
    _id   <- o .: "Id"
    path  <- o .: "Path"
    title <- o .: "Title"
    return Path {..}

instance ToJSON Path

instance Service Path where
  endpoint _ = "paths"
