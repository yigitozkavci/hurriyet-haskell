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
import Control.Monad.Reader


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

type ServiceResponse a = Reader Client (IO (Either String a))

{-| Get a single page
-}
getPage :: Id -> ServiceResponse Page
getPage id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show PageResource id)

{-| Get all pages
-}
getPages :: ServiceResponse [Page]
getPages = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List PageResource)

{-| Get a single news photo gallery
-}
getNewsPhotoGallery :: Id -> ServiceResponse NewsPhotoGallery
getNewsPhotoGallery id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show NewsPhotoGalleryResource id)

{-| Get all news photo galleries
-}
getNewsPhotoGalleries :: ServiceResponse [NewsPhotoGallery]
getNewsPhotoGalleries = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List NewsPhotoGalleryResource)

{-| Get a single column
-}
getColumn :: Id -> ServiceResponse Column
getColumn id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show ColumnResource id)

{-| Get all columns
-}
getColumns :: ServiceResponse [Column]
getColumns = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List ColumnResource)

{-| Get a single path
-}
getPath :: Id -> ServiceResponse Path
getPath id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show PathResource id)

{-| Get all paths
-}
getPaths :: ServiceResponse [Path]
getPaths = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List PathResource)

{-| Get a single writer
-}
getWriter :: Id -> ServiceResponse Writer
getWriter id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show WriterResource id)

{-| Get all writers
-}
getWriters :: ServiceResponse [Writer]
getWriters = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List WriterResource)

{-| Get single article
-}
getArticle :: Id -> ServiceResponse Article
getArticle id = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (Show ArticleResource id)

{-| Get all articles
-}
getArticles :: ServiceResponse [Article]
getArticles = do
  client <- ask
  return $ eitherDecode <$> fetchResource client (List ArticleResource)

withClient :: Client -> Reader Client a -> a
withClient client reader = runReader reader client
