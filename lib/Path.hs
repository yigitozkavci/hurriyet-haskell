{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Path where

import Data.Aeson
import GHC.Generics

{- TODO: Add files -}
data Path = Path
  { _id          :: String
  , path         :: String
  , title        :: String
  } deriving (Generic, Show)

instance FromJSON Path where
  parseJSON = withObject "path" $ \o -> do
    _id   <- o .: "Id"
    path  <- o .: "Path"
    title <- o .: "Title"
    return Path {..}

instance ToJSON Path

