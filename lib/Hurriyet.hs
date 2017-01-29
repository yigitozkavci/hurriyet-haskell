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
import Data.Aeson (decode, eitherDecode)

apiKey :: I.ByteString
apiKey =
  "9a9dcb9808624ac69e6c557a192afb33"

articlesUrl :: String
articlesUrl =
  "https://api.hurriyet.com.tr/v1/articles"

type Id = String

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

data Operation = List Resource
               | Show Resource Id

baseUrl :: String
baseUrl =
  "https://api.hurriyet.com.tr/v1/"

getUrl :: Operation -> String
getUrl (List resource) =
  baseUrl ++ show resource
getUrl (Show resource id) =
  baseUrl ++ show resource ++ "/" ++ id

fetchResource :: Operation -> IO L.ByteString
fetchResource operation = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ getUrl operation
  let request = initialRequest { method = "GET", requestHeaders = [("apikey", apiKey)] }
  response <- httpLbs request manager
  return $ responseBody response

getPage :: Id -> IO (Either String Page)
getPage id =
  fetchResource (Show PageResource id) >>= \str ->
    return $ eitherDecode str

-- TODO: DRY these methods up
getPages :: IO (Either String [Page])
getPages =
  fetchResource (List PageResource) >>= \str ->
    return $ eitherDecode str

getNewsPhotoGallery :: Id -> IO (Either String NewsPhotoGallery)
getNewsPhotoGallery id =
  fetchResource (Show NewsPhotoGalleryResource id) >>= \str ->
    return $ eitherDecode str

getNewsPhotoGalleries :: IO (Either String [NewsPhotoGallery])
getNewsPhotoGalleries =
  fetchResource (List NewsPhotoGalleryResource) >>= \str ->
    return $ eitherDecode str

getColumn :: Id -> IO (Either String Column)
getColumn id =
  fetchResource (Show ColumnResource id) >>= \str ->
    return $ eitherDecode str

getColumns :: IO (Either String [Column])
getColumns =
  fetchResource (List ColumnResource) >>= \str ->
    return $ eitherDecode str

getPath :: Id -> IO (Either String Path)
getPath id =
  fetchResource (Show PathResource id) >>= \str ->
    return $ eitherDecode str

getPaths :: IO (Either String [Path])
getPaths =
  fetchResource (List PathResource) >>= \str ->
    return $ eitherDecode str

getWriter :: Id -> IO (Either String Writer)
getWriter id =
  fetchResource (Show WriterResource id) >>= \str ->
    return $ eitherDecode str

getWriters :: IO (Either String [Writer])
getWriters =
  fetchResource (List WriterResource) >>= \str ->
    return $ eitherDecode str

getArticle :: Id -> IO (Either String Article)
getArticle id =
  fetchResource (Show ArticleResource id) >>= \str ->
    return $ eitherDecode str

getArticles :: IO (Either String [Article])
getArticles =
  fetchResource (List ArticleResource) >>= \str ->
    return $ eitherDecode str
