{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Page where
  import Data.Aeson
  import GHC.Generics
  import Data.ByteString.Lazy (ByteString, pack)

  {- TODO: Add relatedNews -}
  data Page = Page
    { _id :: String
    , createdDate :: String
    , title :: String
    , url :: String
    } deriving (Generic, Show)

  instance FromJSON Page where
    parseJSON = withObject "page" $ \o -> do
      _id          <- o .: "Id"
      createdDate  <- o .: "CreatedDate"
      title        <- o .: "Title"
      url          <- o .: "Url"
      return Page {..}
