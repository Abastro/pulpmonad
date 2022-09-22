module Main (main) where

import Defines
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "mySpaces" $ do
    it "has length 9" $ length mySpaces `shouldBe` 9
