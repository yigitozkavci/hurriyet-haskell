module Service where

class Service s where
  endpoint :: s -> String
