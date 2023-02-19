{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Event.EntrySpec
import Control.Event.StateSpec
#ifdef USE_HARDWARE
import Status.HWSpec
#endif
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Control" $ do
    describe "Event" $ do
      describe "Entry" entrySpec
      describe "State" stateSpec
#ifdef USE_HARDWARE
  describe "Status" $ do
    hwSpec
#endif
