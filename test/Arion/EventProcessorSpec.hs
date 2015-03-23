{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessorSpec where

import Test.Hspec
import System.FSNotify
import Data.Time.Clock
import Data.Time.Calendar
import Data.Map

import Arion.EventProcessor
import Arion.Types

main :: IO ()
main = hspec spec

spec = do
    describe "Event Processor" $ do
        it "responds to a Modified event on a test file by creating commands to run tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let modifiedEvent = Modified "mydir/ModuleASpec.hs" sampleTime
            let expectedCommands = [Command "runhaskell -isrc mydir/ModuleASpec.hs"]

            processEvent sourceToTestFileMap modifiedEvent `shouldBe` expectedCommands
        it "responds to a Modified event on a source file by creating commands to run the associated tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let modifiedEvent = Modified "src/ModuleA.hs" sampleTime
            let expectedCommands = [Command "runhaskell -isrc test/ModuleASpec.hs",
                                    Command "runhaskell -isrc test/ModuleBSpec.hs"]


            processEvent sourceToTestFileMap modifiedEvent `shouldBe` expectedCommands
        it "responds to a Added event on a test file by creating commands to run tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let addedEvent = Added "mydir/ModuleASpec.hs" sampleTime
            let expectedCommands = [Command "runhaskell -isrc mydir/ModuleASpec.hs"]

            processEvent sourceToTestFileMap addedEvent `shouldBe` expectedCommands
        it "responds to a Added event on a source file by creating commands to run the associated tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let addedEvent = Added "src/ModuleA.hs" sampleTime
            let expectedCommands = [Command "runhaskell -isrc test/ModuleASpec.hs",
                                    Command "runhaskell -isrc test/ModuleBSpec.hs"]


            processEvent sourceToTestFileMap addedEvent `shouldBe` expectedCommands

sampleTime :: UTCTime
sampleTime = UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 2)

