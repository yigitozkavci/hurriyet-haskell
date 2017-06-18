module Hurriyet.Services.Service
  ( Service(..)
  ) where
--------------------------------------------------------------------------------
import           Data.Aeson (FromJSON)

class FromJSON s => Service s where
  endpoint :: s -> String
