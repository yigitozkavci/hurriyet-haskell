{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
--------------------------------------------------------------------------------
import           Test.Hspec
import           Data.String
import           Data.Aeson
import           Data.String.Here        (here)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy          (Text, pack)
import           Data.Either             (isRight)
import           Data.Monoid             ((<>))
--------------------------------------------------------------------------------
import qualified Hurriyet                as H
import qualified Hurriyet.Services       as HS
--------------------------------------------------------------------------------

articleListResponse :: Text
articleListResponse = [here|
[
  {
    "Id": "40492649",
    "ContentType": "Article",
    "CreatedDate": "2017-06-16T16:44:53Z",
    "Description": "Kargoyla gelen gelinlik kolisi 9 kişiyi hastanelik etti (2)",
    "Files": [],
    "ModifiedDate": "2017-06-16T16:44:53Z",
    "Path": "/yerel-haberler/mugla/",
    "StartDate": "2017-06-16T16:44:53Z",
    "Tags": [],
    "Title": "Kargoyla gelen gelinlik kolisi 9 kişiyi hastanelik etti (2)",
    "Url": "http://www.hurriyet.com.tr/kargoyla-gelen-gelinlik-kolisi-9-kisiyi-hastane-40492649"
  },
  {
    "Id": "40492650",
    "ContentType": "Article",
    "CreatedDate": "2017-06-16T16:38:45Z",
    "Description": "Kitabına 'Başımı Yedin Duşakabinoğlu' ismini verecek",
    "Files": [
      {
        "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/59440b4deb10bb21f84439dc.jpg",
        "Metadata": {
          "Title": "Kitabına 'Başımı Yedin Duşakabinoğlu' ismini verecek",
          "Description": "refid:Kitabına 'Başımı Yedin Duşakabinoğlu' ismini verecek ilişkili resim dosyası"
        }
      }
    ],
    "ModifiedDate": "2017-06-16T16:38:45Z",
    "Path": "/yerel-haberler/denizli/",
    "StartDate": "2017-06-16T16:38:45Z",
    "Tags": [],
    "Title": "Kitabına 'Başımı Yedin Duşakabinoğlu' ismini verecek",
    "Url": "http://www.hurriyet.com.tr/kitabina-basimi-yedin-dusakabinoglu-ismini-ve-40492650"
  }
]
|]

articleShowResponse :: Text
articleShowResponse = [here|
{
  "Id": "40486849",
  "ContentType": "Article",
  "CreatedDate": "2017-06-11T18:39:13Z",
  "Description": "Bingöl'de çatışma: 1 şehit",
  "Editor": "DHA",
  "Files": [],
  "ModifiedDate": "2017-06-11T18:39:13Z",
  "Path": "/yerel-haberler/bingol/",
  "RelatedNews": [],
  "StartDate": "2017-06-11T18:39:13Z",
  "Tags": [],
  "Text": "<p><p>BİNGÖL, (DHA) - BİNGÖL'ün genç ilçesinde teröristlerle çıkan çatışmada 1 uzman çavuş şehit oldu.<br>\r\nGenç İlçesi Yeniyazı bölgesinde akşam saatlerinde güvenlik güçleri ile PKK terör örgütü mensupları arasında sıcak tmas sağlandı. Çatışmada 1 uzman çavuş şehit oldu. Şehit uzman çavuşun cenazesi Bingöl Devlet Hastanesi morguna kaldırılırken, bölgede geniş çaplı operasyon başlatıldı.</p>\r\n</p>\r\n","Title": "Bingöl'de çatışma: 1 şehit",
  "Url": "http://www.hurriyet.com.tr/bingolde-catisma-1-sehit-40486849",
  "Writers":[]
}
|]

columnListResponse :: Text
columnListResponse = [here|
[
  {
    "Id": "40492648",
    "Fullname": "Ateş BAKAN",
    "ContentType": "Column",
    "CreatedDate": "2017-06-16T16:40:48.61Z",
    "Description": "Konya, Aykut Kocaman’ı “Omuzlarda Uğurladı”...",
    "Files": [],
    "Path": "/spor/yazarlar/",
    "StartDate": "2017-06-16T16:41:48.317Z",
    "Title": "Hoş geldin “Kocaman Adam”",
    "Url": "http://www.hurriyet.com.tr/yazarlar/ates-bakan/hos-geldin-kocaman-adam-40492648",
    "WriterId": "55ea09f9f018fbaf44942579"
  },
  {
    "Id": "40491947",
    "Fullname": "Murat Güloğlu",
    "ContentType": "Column",
    "CreatedDate": "2017-06-16T08:34:16.871Z",
    "Description": "Nihayet bohem-burjuva tayfanın bolca takıldığı Bozcaada sezonumu açmış bulunuyorum. Son derece mesudum. ",
    "Files": [
        {
          "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/590b00fec03c0e3c0467c337.jpg",
          "Metadata": {
            "Title": ""
          }
        },
        {
          "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/590b00fec03c0e3c0467c339.jpg",
          "Metadata": {
            "Title": ""
          }
        },
        {
          "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/590b00fec03c0e3c0467c33b.jpg",
          "Metadata": {
            "Title": ""
          }
        }
    ],
    "Path": "/yazarlar/murat-guloglu/",
    "StartDate": "2017-06-16T08:36:00Z",
    "Title": "Gitme Yunan’a, gel Bozcaada’ya!",
    "Url": "http://www.hurriyet.com.tr/yazarlar/murat-guloglu/gitme-yunana-gel-bozcaadaya-40491947",
    "WriterId": "590b00e6c03c0e3c0467c335"
  }
]
|]

columnShowResponse :: Text
columnShowResponse = [here|
{
    "Id": "40491725",
    "Fullname": "Erdem Cürgen",
    "ContentType": "Column",
    "CreatedDate": "2017-06-16T06:02:19.347Z",
    "Description": "Ülkemizde ve Avrupa’nın önde gelen liglerinde sezonları tamamladık. Şu an İspanya...",
    "Files": [
        {
            "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/58dbaa017152d83cb4ece964.jpg",
            "Metadata": {
                "Title": ""
            }
        },
        {
            "FileUrl": "http://i.hurimg.com/i/hurriyet/98/620x0/58dbaa0d7152d83cb4ece966.jpg",
            "Metadata": {
                "Title": ""
            }
        }
    ],
    "Path": "/yazarlar/erdem-curgen/",
    "StartDate": "2017-06-16T06:07:00Z",
    "Text": "<p><img src=\"http://i.hurimg.com/i/hurriyet/98/770x0/59437536c9de3d1bfce98b74\" width=\"100%\"></p...",
    "Title": "Ligler bitti, şimdi ne izleyeceğiz",
    "Url": "http://www.hurriyet.com.tr/yazarlar/erdem-curgen/ligler-bitti-simdi-ne-izleyecegiz-40491725"
}
|]

testApiKey :: IsString s => s
testApiKey = "some_api_key"

testClient :: H.Client
testClient = H.getClient testApiKey

{- Beware: anyone using this function should annotate return
 - value, since we are doing json decoding.
 -}
testDecoding :: FromJSON a => Text -> IO a
testDecoding str = do
  let result = eitherDecode (encodeUtf8 str)
  isRight result `shouldBe` True
  return $ fromRight result

fromRight :: Show a => Either a b -> b
fromRight (Left x) = error $ "Should not have got a left value: " ++ show x
fromRight (Right x) = x

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
          _ <- testDecoding resp :: IO [HS.Article]
          return ()
      context "show" $ do
        it "generates url right" $ do
          let op = H.Show H.ArticleResource "2"
          H.getUrl op `shouldBe` H.baseUrl ++ "articles/2"
        it "parses response" $
          isRight (eitherDecode (encodeUtf8 articleShowResponse) :: Either String HS.Article) `shouldBe` True
    context "column" $ do
      it "has the right endpoint" $
        show H.ColumnResource `shouldBe` "columns"
      context "list" $ do
        it "generates url right" $ do
          let op = H.List H.ColumnResource
          H.getUrl op `shouldBe` H.baseUrl ++ "columns"
        it "parses response" $ do
          _ <- testDecoding columnListResponse  :: IO [HS.Column]
          return ()
      context "show" $ do
        it "generates url right" $ do
          let op = H.Show H.ColumnResource "3"
          H.getUrl op `shouldBe` H.baseUrl ++ "columns/3"
        it "parses response" $
          isRight (eitherDecode (encodeUtf8 articleShowResponse) :: Either String HS.Article) `shouldBe` True
