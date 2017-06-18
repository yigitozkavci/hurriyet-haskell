{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--------------------------------------------------------------------------------
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Data.Text                 (Text)
import           Data.ByteString.Lazy      as L
import           Data.ByteString.Internal  as I
import           Data.ByteString.Char8     as C
import           Data.Aeson                (decode, eitherDecode, FromJSON)
import           Control.Monad.Reader
import           Data.Proxy                (Proxy(..))
--------------------------------------------------------------------------------
import           Hurriyet.Services
import           Hurriyet.Services.Service (Service, endpoint)
--------------------------------------------------------------------------------

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

getEndpoint :: forall proxy a. (Service a) => proxy a -> String
getEndpoint _ = endpoint (undefined :: a)

{-| For now, Hurriyet API only consists of 2 operations. As we have
    more, new operations will be added into here
-}
data Operation = List
               | Show Id

{-| Base url of the Hurriyet API
-}
baseUrl :: String
baseUrl =
  "https://api.hurriyet.com.tr/v1/"

{-| This method constructs url given the operation. Operation already
    contains enough data to construct the url
-}
getUrl :: Operation -> String -> String
getUrl operation endpoint =
  case operation of
    List    -> baseUrl ++ endpoint
    Show id -> baseUrl ++ endpoint ++ "/" ++ id

{-| Given a client and a operation, this method can conquer the world. But prefers
    not to and returns a bytestring representing json string of the response body
-}
fetchResource :: forall proxy a. (Service a) => proxy a -> Client -> Operation -> IO L.ByteString
fetchResource _ client operation = do
  manager <- newManager tlsManagerSettings
  let endpoint = getEndpoint (Proxy :: Proxy a)
  initialRequest <- parseRequest $ getUrl operation endpoint
  let request = initialRequest { method = "GET", requestHeaders = [("apikey", C.pack $ apiKey client)] }
  response <- httpLbs request manager
  return $ responseBody response

type ServiceResponse a = Reader Client (IO (Either String a))

getResource :: forall proxy a. (Service a) => proxy a -> Operation -> ServiceResponse a
getResource _ op = do
  client <- ask
  return (eitherDecode <$> fetchResource (Proxy :: Proxy a) client op :: IO (Either String a))

getResources :: forall proxy a. (Service a) => proxy a -> Operation -> ServiceResponse [a]
getResources _ op = do
  client <- ask
  return (eitherDecode <$> fetchResource (Proxy :: Proxy a) client op :: IO (Either String [a]))

{-| Get a single page
-}
getPage :: Id -> ServiceResponse Page
getPage id = getResource (Proxy :: Proxy Page) (Show id)

{-| Get all pages
-}
getPages :: ServiceResponse [Page]
getPages = getResources (Proxy :: Proxy Page) List

{-| Get a single news photo gallery
-}
getNewsPhotoGallery :: Id -> ServiceResponse NewsPhotoGallery
getNewsPhotoGallery id = getResource (Proxy :: Proxy NewsPhotoGallery) (Show id)

{-| Get all news photo galleries
-}
getNewsPhotoGalleries :: ServiceResponse [NewsPhotoGallery]
getNewsPhotoGalleries = getResources (Proxy :: Proxy NewsPhotoGallery) List

{-| Get a single column
-}
getColumn :: Id -> ServiceResponse Column
getColumn id = getResource (Proxy :: Proxy Column) (Show id)

{-| Get all columns
-}
getColumns :: ServiceResponse [Column]
getColumns = getResources (Proxy :: Proxy Column) List

{-| Get a single path
-}
getPath :: Id -> ServiceResponse Path
getPath id = getResource (Proxy :: Proxy Path) (Show id)

{-| Get all paths
-}
getPaths :: ServiceResponse [Path]
getPaths = getResources (Proxy :: Proxy Path) List

{-| Get a single writer
-}
getWriter :: Id -> ServiceResponse Writer
getWriter id = getResource (Proxy :: Proxy Writer) (Show id)

{-| Get all writers
-}
getWriters :: ServiceResponse [Writer]
getWriters = getResources (Proxy :: Proxy Writer) List

{-| Get single article
-}
getArticle :: Id -> ServiceResponse Article
getArticle id = getResource (Proxy :: Proxy Article) (Show id)

{-| Get all articles
-}
getArticles :: ServiceResponse [Article]
getArticles = getResources (Proxy :: Proxy Article) List

withClient :: Client -> Reader Client a -> a
withClient client reader = runReader reader client
