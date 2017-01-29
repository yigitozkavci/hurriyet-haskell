{-# LANGUAGE OverloadedStrings #-}
module Hurriyet where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text (Text)
import Data.ByteString.Lazy as L
import Data.ByteString.Internal as I
import Data.ByteString.Char8 as C
import Hurriyet.Services
import Data.Aeson (decode, eitherDecode)


{- Planned usage:
   defaultOptions :: Options
   getClient :: I.ByteString -> Client
   getArticles :: Client -> IO (Either String [Articles])
-}

type Id = String

newtype Client = Client
  { apiKey :: String
  }

getClient :: String -> Client
getClient = Client

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
getUrl operation =
  case operation of
    List resource    -> baseUrl ++ show resource
    Show resource id -> baseUrl ++ show resource ++ "/" ++ id

fetchResource :: Client -> Operation -> IO L.ByteString
fetchResource client operation = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ getUrl operation
  let request = initialRequest { method = "GET", requestHeaders = [("apikey", C.pack $ apiKey client)] }
  response <- httpLbs request manager
  return $ responseBody response

getPage :: Client -> Id -> IO (Either String Page)
getPage client id =
  fetchResource client (Show PageResource id) >>= \str ->
    return $ eitherDecode str

getPages :: Client -> IO (Either String [Page])
getPages client =
  fetchResource client (List PageResource) >>= \str ->
    return $ eitherDecode str

getNewsPhotoGallery :: Client -> Id -> IO (Either String NewsPhotoGallery)
getNewsPhotoGallery client id =
  fetchResource client (Show NewsPhotoGalleryResource id) >>= \str ->
    return $ eitherDecode str

getNewsPhotoGalleries :: Client -> IO (Either String [NewsPhotoGallery])
getNewsPhotoGalleries client =
  fetchResource client (List NewsPhotoGalleryResource) >>= \str ->
    return $ eitherDecode str

getColumn :: Client -> Id -> IO (Either String Column)
getColumn client id =
  fetchResource client (Show ColumnResource id) >>= \str ->
    return $ eitherDecode str

getColumns :: Client -> IO (Either String [Column])
getColumns client =
  fetchResource client (List ColumnResource) >>= \str ->
    return $ eitherDecode str

getPath :: Client -> Id -> IO (Either String Path)
getPath client id =
  fetchResource client (Show PathResource id) >>= \str ->
    return $ eitherDecode str

getPaths :: Client -> IO (Either String [Path])
getPaths client =
  fetchResource client (List PathResource) >>= \str ->
    return $ eitherDecode str

getWriter :: Client -> Id -> IO (Either String Writer)
getWriter client id =
  fetchResource client (Show WriterResource id) >>= \str ->
    return $ eitherDecode str

getWriters :: Client -> IO (Either String [Writer])
getWriters client =
  fetchResource client (List WriterResource) >>= \str ->
    return $ eitherDecode str

getArticle :: Client -> Id -> IO (Either String Article)
getArticle client id =
  fetchResource client (Show ArticleResource id) >>= \str ->
    return $ eitherDecode str

getArticles :: Client -> IO (Either String [Article])
getArticles client =
  fetchResource client (List ArticleResource) >>= \str ->
    return $ eitherDecode str
