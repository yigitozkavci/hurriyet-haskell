{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
--------------------------------------------------------------------------------
import           Test.Hspec
import           Data.String
import           Data.Aeson
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy          (Text, pack)
import           Data.Either             (isLeft)
import           Data.Monoid             ((<>))
import           Control.Monad           (when)
import           Data.Proxy              (Proxy(..))
import           Data.Foldable           (forM_)
--------------------------------------------------------------------------------
import qualified Hurriyet                as H
import qualified Hurriyet.Services       as HS
--------------------------------------------------------------------------------

testApiKey :: IsString s => s
testApiKey = "some_api_key"

testClient :: H.Client
testClient = H.getClient testApiKey

{- Beware: anyone using this function should annotate return
 - value, since we are doing json decoding.
 -}
testDecoding :: forall proxy a. (Show a, FromJSON a) => proxy a -> Text -> IO ()
testDecoding _ str = do
  let result = eitherDecode (encodeUtf8 str) :: Either String a
  when (isLeft result) $
    expectationFailure $ "Could not decode service: " <> fromLeft result
  return ()

fromLeft :: Show b => Either a b -> a
fromLeft (Right x) = error $ "Should not have got a right value: " ++ show x
fromLeft (Left x) = x

data Operation = List H.Resource
               | Show H.Resource

stubbedResponseFilename :: Operation -> String
stubbedResponseFilename (List resource) = show resource <> "_list_response"
stubbedResponseFilename (Show resource) = show resource <> "_show_response"

stubbedResponse :: Operation -> IO Text
stubbedResponse op =
  let
    filepath = "tests/static/" <> stubbedResponseFilename op <> ".json"
  in
    pack <$> readFile filepath 

serviceTestMapping ::
  [
    ( String                       -- Name of the resource test. Will be used as description
    , ( (H.Operation, String)      -- Endpoint check for list operation
      , (H.Operation, String)      -- Endpoint check for show operation
      , (Operation, Text -> IO ()) -- List operation and its response decoder
      , (Operation, Text -> IO ()) -- Show operation and its response decoder
      )
    )
  ]

serviceTestMapping =
  [ ("article", ( (H.List H.ArticleResource, "articles")
                , (H.Show H.ArticleResource "3", "articles/3")
                , (List H.ArticleResource, testDecoding (Proxy :: Proxy [HS.Article]))
                , (Show H.ArticleResource, testDecoding (Proxy :: Proxy HS.Article))
                )
    )
  , ("column", ( (H.List H.ColumnResource, "columns")
                , (H.Show H.ColumnResource "3", "columns/3")
                , (List H.ColumnResource, testDecoding (Proxy :: Proxy [HS.Column]))
                , (Show H.ColumnResource, testDecoding (Proxy :: Proxy HS.Column))
                )
    )
  , ("news_photo_gallery", ( (H.List H.NewsPhotoGalleryResource, "newsphotogalleries")
                           , (H.Show H.NewsPhotoGalleryResource "3", "newsphotogalleries/3")
                           , (List H.NewsPhotoGalleryResource, testDecoding (Proxy :: Proxy [HS.NewsPhotoGallery]))
                           , (Show H.NewsPhotoGalleryResource, testDecoding (Proxy :: Proxy HS.NewsPhotoGallery))
                           )
    )
  -- Pages endpoint is inaccessible at the moment of testing
  -- , ("page", ( (H.List H.PageResource, "pages")
  --            , (H.Show H.PageResource "3", "pages/3")
  --            , (List H.PageResource, testDecoding (Proxy :: Proxy [HS.Page]))
  --            , (Show H.PageResource, testDecoding (Proxy :: Proxy HS.Page))
  --            )
  --   )
  , ("path", ( (H.List H.PathResource, "paths")
             , (H.Show H.PathResource "3", "paths/3")
             , (List H.PathResource, testDecoding (Proxy :: Proxy [HS.Path]))
             , (Show H.PathResource, testDecoding (Proxy :: Proxy HS.Path))
             )
    )
  , ("writer", ( (H.List H.WriterResource, "writers")
             , (H.Show H.WriterResource "3", "writers/3")
             , (List H.WriterResource, testDecoding (Proxy :: Proxy [HS.Writer]))
             , (Show H.WriterResource, testDecoding (Proxy :: Proxy HS.Writer))
             )
    )
  ]

main :: IO ()
main = hspec $ do
  describe "client" $ do
    it "saves the api key" $
      H.apiKey testClient `shouldBe` testApiKey
    it "reads" $ do
      _ <- H.withClient testClient $ do
        articles <- H.getArticles
        _article <- H.getArticle "204201"
        return articles
      return ()  
  describe "resources" $
    forM_ serviceTestMapping $ \resource ->
      context (fst resource) $ do
        let (listUrl, showUrl, (listOperation, listDecodeTester), (showOperation, showDecodeTester)) = snd resource
        context "list" $
          it "has the right endpoint" $
            H.getUrl (fst listUrl) `shouldBe` H.baseUrl ++ snd listUrl 
        it "parses response" $
          listDecodeTester =<< stubbedResponse listOperation
        context "show" $
          it "has the right endpoint" $
            H.getUrl (fst showUrl) `shouldBe` H.baseUrl ++ snd showUrl 
        it "parses response" $
          showDecodeTester =<< stubbedResponse showOperation
  
