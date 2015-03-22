{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessorSpec where

import Test.Hspec
import System.FSNotify
import Data.Time.Clock
import Data.Time.Calendar

import Arion.EventProcessor
import Arion.Types

main :: IO ()
main = hspec spec

spec = do
    describe "Event Processor" $ do
        it "responds to a Modified event on a test file by creating a command to run tests" $ do
            let modifiedEvent = Modified "mydir/SampleSpec.hs" sampleTime
            let expectedCommand = Command "runhaskell mydir/SampleSpec.hs"

            processEvent modifiedEvent `shouldBe` expectedCommand

sampleTime :: UTCTime
sampleTime = UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 2)
