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

instance Show Resource where
  show ArticleResource = "articles"
  show PageResource = "pages"
  show NewsPhotoGalleryResource = "newsphotogalleries"

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

debugPrint :: IO ()
debugPrint =
  getPages >>= print
