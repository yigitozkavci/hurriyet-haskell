{-# LANGUAGE OverloadedStrings #-}
module Hurriyet where
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text (Text)
import Data.ByteString.Lazy as L
import Data.ByteString.Internal as I
import Article
import Page
import NewsPhotoGallery
import Column
import Path
import Writer
import Data.Aeson (decode)

apiKey :: I.ByteString
apiKey =
  "9a9dcb9808624ac69e6c557a192afb33"

articlesUrl :: String
articlesUrl =
  "https://api.hurriyet.com.tr/v1/articles"

data Resource = ArticleResource
              | PageResource
              | NewsPhotoGalleryResource
              | ColumnResource
              | PathResource
              | WriterResource

instance Show Resource where
  show ArticleResource          = "articles"
  show PageResource             = "pages"
  show NewsPhotoGalleryResource = "newsphotogalleries"
  show ColumnResource           = "columns"
  show PathResource             = "paths"
  show WriterResource           = "writers"

baseUrl :: String
baseUrl =
  "https://api.hurriyet.com.tr/v1/"

getUrl :: Resource -> String
getUrl resource =
  baseUrl ++ show resource

fetchResource :: Resource -> IO L.ByteString
fetchResource resource = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ getUrl resource
  let request = initialRequest { method = "GET", requestHeaders = [("apikey", apiKey)] }
  response <- httpLbs request manager
  return $ responseBody response

getPages :: IO (Maybe [Page])
getPages =
  fetchResource PageResource >>= \str ->
    return $ decode str

getNewsPhotoGalleries :: IO (Maybe [NewsPhotoGallery])
getNewsPhotoGalleries =
  fetchResource NewsPhotoGalleryResource >>= \str ->
    return $ decode str

getArticles :: IO (Maybe [Article])
getArticles =
  fetchResource ArticleResource >>= \str ->
    return $ decode str

getColumns :: IO (Maybe [Column])
getColumns =
  fetchResource ColumnResource >>= \str ->
    return $ decode str

getPaths :: IO (Maybe [Path])
getPaths =
  fetchResource PathResource >>= \str ->
    return $ decode str

getWriters :: IO (Maybe [Writer])
getWriters =
  fetchResource WriterResource >>= \str ->
    return $ decode str
