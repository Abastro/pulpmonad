{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef USE_HARDWARE
import System.HWSpec
#endif
import Reactive.EntrySpec
import Reactive.StateSpec
import System.HardwareSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Reactive" $ do
    describe "Entry" entrySpec
    describe "State" stateSpec
  describe "System" $ do
    describe "Hardware" hardwareSpec
#ifdef USE_HARDWARE
  describe "System" $ do
    hwSpec
#endif
