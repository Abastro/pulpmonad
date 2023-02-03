module Main (main) where

import Control.ReactiveSpec
import Status.HWSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Control" $ do
    reactiveSpec
  describe "Status" $ do
    hwSpec
