module Main (main) where

import Control.Event.EntrySpec
import Control.Event.StateSpec
import Status.HWSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Control" $ do
    describe "Event" $ do
      describe "Entry" entrySpec
      describe "State" stateSpec
  describe "Status" $ do
    hwSpec
