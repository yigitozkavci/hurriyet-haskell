module Main where
 
import Hurriyet
import Test.Hspec
 
main :: IO ()
main = hspec $
  describe "apiKey returns api key string" $
    it "returns" $
      take 1 (show apiKey) `shouldBe` "\""
