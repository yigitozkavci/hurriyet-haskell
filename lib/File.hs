module File where

import Data.Aeson
import GHC.Generics

newtype File = File
  { fileUrl :: String
  }

instance FromJSON File where
  parseJSON = withObject "file" $ \o -> do
    fileUrl <- o .: "FileUrl"
    return File{..}

instance ToJSON File
