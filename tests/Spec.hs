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

main :: IO ()
main = hspec $ do
  describe "client" $
    it "saves the api key" $
      H.apiKey testClient `shouldBe` testApiKey
  describe "resources" $ do
    context "article" $ do
      it "has the right endpoint" $
        show H.ArticleResource `shouldBe` "articles"
      context "list" $ do
        it ("generates " <> "url" <> " right") $ do
          let op = H.List H.ArticleResource
          H.getUrl op `shouldBe` H.baseUrl ++ "articles"
        it "parses response" $ do
          resp <- stubbedResponse (List H.ArticleResource)
          testDecoding (Proxy :: Proxy [HS.Article]) resp
          return ()
      context "show" $ do
        it "generates url right" $ do
          let op = H.Show H.ArticleResource "2"
          H.getUrl op `shouldBe` H.baseUrl ++ "articles/2"
        it "parses response" $ do
          resp <- stubbedResponse (Show H.ArticleResource)
          testDecoding (Proxy :: Proxy HS.Article) resp
          return ()
    context "column" $ do
      it "has the right endpoint" $
        show H.ColumnResource `shouldBe` "columns"
      context "list" $ do
        it "generates url right" $ do
          let op = H.List H.ColumnResource
          H.getUrl op `shouldBe` H.baseUrl ++ "columns"
        it "parses response" $ do
          resp <- stubbedResponse (List H.ColumnResource)
          testDecoding (Proxy :: Proxy [HS.Column]) resp
          return ()
      context "show" $ do
        it "generates url right" $ do
          let op = H.Show H.ColumnResource "3"
          H.getUrl op `shouldBe` H.baseUrl ++ "columns/3"
        it "parses response" $ do
          resp <- stubbedResponse (Show H.ColumnResource)
          testDecoding (Proxy :: Proxy HS.Column) resp
          return ()
