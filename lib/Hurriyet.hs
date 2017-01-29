{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hurriyet
Copyright   : (c) Yiğit Özkavcı, 2017
License     : WTFPL
Maintainer  : yigitozkavci8@gmail.com
Stability   : experimental
Portability : GHC

This module controls all the data flow and client interface required to interact with the library.
-}
module Hurriyet where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text (Text)
import Data.ByteString.Lazy as L
import Data.ByteString.Internal as I
import Data.ByteString.Char8 as C
import Hurriyet.Services
import Data.Aeson (decode, eitherDecode)


{- Planned client usage:
   defaultOptions :: Options
   getClient :: I.ByteString -> Client
   getArticles :: Client -> IO (Either String [Articles])
-}

{-| Type synonym for Id which will be used for endpoints
-}
type Id = String

{-| Type synonym for api key
-}
type ApiKey = String

{-| Client will be used and passed through every
    API call
-}
newtype Client = Client
  { apiKey :: ApiKey
  }

{-| This is how you construct the client. Takes apiKey as
    an argument
-}
getClient :: String -> Client
getClient = Client

{-| Each of these resources represent services. These are
    used for passing service-spesific data such as endpoint string
-}
data Resource = ArticleResource
              | PageResource
              | NewsPhotoGalleryResource
              | ColumnResource
              | PathResource
              | WriterResource

{-| Each resource represents an endpoint string
-}
instance Show Resource where
  show ArticleResource          = "articles"
  show PageResource             = "pages"
  show NewsPhotoGalleryResource = "newsphotogalleries"
  show ColumnResource           = "columns"
  show PathResource             = "paths"
  show WriterResource           = "writers"

{-| For now, Hurriyet API only consists of 2 operations. As we have
    more, new operations will be added into here
-}
data Operation = List Resource
               | Show Resource Id

{-| Base url of the Hurriyet API
-}
baseUrl :: String
baseUrl =
  "https://api.hurriyet.com.tr/v1/"

{-| This method constructs url given the operation. Operation already
    contains enough data to construct the url
-}
getUrl :: Operation -> String
getUrl operation =
  case operation of
    List resource    -> baseUrl ++ show resource
    Show resource id -> baseUrl ++ show resource ++ "/" ++ id

{-| Given a client and a operation, this method can conquer the world. But prefers
    not to and returns a bytestring representing json string of the response body
-}
fetchResource :: Client -> Operation -> IO L.ByteString
fetchResource client operation = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ getUrl operation
  let request = initialRequest { method = "GET", requestHeaders = [("apikey", C.pack $ apiKey client)] }
  response <- httpLbs request manager
  return $ responseBody response

{-| Get a single page
-}
getPage :: Client -> Id -> IO (Either String Page)
getPage client id =
  fetchResource client (Show PageResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all pages
-}
getPages :: Client -> IO (Either String [Page])
getPages client =
  fetchResource client (List PageResource) >>= \str ->
    return $ eitherDecode str

{-| Get a single news photo gallery
-}
getNewsPhotoGallery :: Client -> Id -> IO (Either String NewsPhotoGallery)
getNewsPhotoGallery client id =
  fetchResource client (Show NewsPhotoGalleryResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all news photo galleries
-}
getNewsPhotoGalleries :: Client -> IO (Either String [NewsPhotoGallery])
getNewsPhotoGalleries client =
  fetchResource client (List NewsPhotoGalleryResource) >>= \str ->
    return $ eitherDecode str

{-| Get a single column
-}
getColumn :: Client -> Id -> IO (Either String Column)
getColumn client id =
  fetchResource client (Show ColumnResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all columns
-}
getColumns :: Client -> IO (Either String [Column])
getColumns client =
  fetchResource client (List ColumnResource) >>= \str ->
    return $ eitherDecode str

{-| Get a single path
-}
getPath :: Client -> Id -> IO (Either String Path)
getPath client id =
  fetchResource client (Show PathResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all paths
-}
getPaths :: Client -> IO (Either String [Path])
getPaths client =
  fetchResource client (List PathResource) >>= \str ->
    return $ eitherDecode str

{-| Get a single writer
-}
getWriter :: Client -> Id -> IO (Either String Writer)
getWriter client id =
  fetchResource client (Show WriterResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all writers
-}
getWriters :: Client -> IO (Either String [Writer])
getWriters client =
  fetchResource client (List WriterResource) >>= \str ->
    return $ eitherDecode str

{-| Get single article
-}
getArticle :: Client -> Id -> IO (Either String Article)
getArticle client id =
  fetchResource client (Show ArticleResource id) >>= \str ->
    return $ eitherDecode str

{-| Get all articles
-}
getArticles :: Client -> IO (Either String [Article])
getArticles client =
  fetchResource client (List ArticleResource) >>= \str ->
    return $ eitherDecode str
