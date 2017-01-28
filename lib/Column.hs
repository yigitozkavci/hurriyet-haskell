{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Column where

import Data.Aeson
import GHC.Generics

{- TODO: Add files -}
data Column = Column
  { _id          :: String
  , fullName     :: String
  , contentType  :: String
  , createdDate  :: String
  , description  :: String
  , path         :: String
  , startDate    :: String
  , title        :: String
  , url          :: String
  , writerId     :: String
  } deriving (Generic, Show)

instance FromJSON Column where
  parseJSON = withObject "column" $ \o -> do
    _id          <- o .: "Id"
    fullName     <- o .: "Fullname"
    contentType  <- o .: "ContentType"
    createdDate  <- o .: "CreatedDate"
    description  <- o .: "Description"
    path         <- o .: "Path"
    startDate    <- o .: "StartDate"
    title        <- o .: "Title"
    url          <- o .: "Url"
    writerId     <- o .: "WriterId"
    return Column {..}

instance ToJSON Column
