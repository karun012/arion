{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessorSpec where

import           Data.Map
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.FSNotify
import           Test.Hspec

import           Arion.EventProcessor
import           Arion.Types
import           Data.Maybe           (fromJust)

main :: IO ()
main = hspec spec

spec = do
    describe "Event Processor" $ do
        it "responds to a Modified event on a test file by creating commands to run tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let modifiedEvent = fromJust . respondToEvent $ Modified "mydir/ModuleASpec.hs" sampleTime

            processEvent sourceToTestFileMap "src" "test" modifiedEvent `shouldBe` [Echo "mydir/ModuleASpec.hs changed", RunHaskell "src" "test" "mydir/ModuleASpec.hs"]
        it "responds to a Modified event on a source file by creating commands to run the associated tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let modifiedEvent = fromJust . respondToEvent $
                                Modified "src/ModuleA.hs" sampleTime

            processEvent sourceToTestFileMap "src" "test" modifiedEvent `shouldBe` [Echo "src/ModuleA.hs changed", RunHaskell "src" "test" "test/ModuleASpec.hs",
                                                                                    RunHaskell "src" "test" "test/ModuleBSpec.hs"]
        it "responds to a Added event on a test file by creating commands to run tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let addedEvent = fromJust . respondToEvent $ Added "mydir/ModuleASpec.hs" sampleTime

            processEvent sourceToTestFileMap "src" "test" addedEvent `shouldBe` [Echo "mydir/ModuleASpec.hs changed", RunHaskell "src" "test" "mydir/ModuleASpec.hs"]
        it "responds to a Added event on a source file by creating commands to run the associated tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let addedEvent = fromJust . respondToEvent $ Added "src/ModuleA.hs" sampleTime

            processEvent sourceToTestFileMap "src" "test" addedEvent `shouldBe` [Echo "src/ModuleA.hs changed", RunHaskell "src" "test" "test/ModuleASpec.hs",
                                                                                        RunHaskell "src" "test" "test/ModuleBSpec.hs"]
        it "ignores non haskell source files" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let testFileA = TestFile "test/ModuleASpec.hs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.hs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let modifiedEvent = fromJust . respondToEvent $ Modified "mydir/ModuleASpec.hs~" sampleTime

            processEvent sourceToTestFileMap "src" "test" modifiedEvent `shouldBe` []

            let addedEvent = fromJust . respondToEvent $ Added "mydir/ModuleASpec.swp" sampleTime

            processEvent sourceToTestFileMap "src" "test" addedEvent `shouldBe` []
        it "does not ignore lhs files" $ do
            let sourceFilePathA = "src/ModuleA.lhs"
            let testFileA = TestFile "test/ModuleASpec.lhs" ["ModuleA"]
            let testFileB = TestFile "test/ModuleBSpec.lhs" ["ModuleB"]
            let sourceToTestFileMap = fromList [(sourceFilePathA, [testFileA, testFileB])]
            let addedEvent = fromJust . respondToEvent $ Added "mydir/ModuleASpec.lhs" sampleTime

            processEvent sourceToTestFileMap "src" "test" addedEvent `shouldBe` [Echo "mydir/ModuleASpec.lhs changed", RunHaskell "src" "test" "mydir/ModuleASpec.lhs"]
        it "tells you when a source file does not have any tests" $ do
            let sourceFilePathA = "src/ModuleA.hs"
            let sourceToTestFileMap = fromList [(sourceFilePathA, [])]
            let addedEvent = fromJust . respondToEvent $ Added "src/ModuleA.hs" sampleTime

            processEvent sourceToTestFileMap "src" "test" addedEvent `shouldBe` [Echo "src/ModuleA.hs changed", Echo "src/ModuleA.hs does not have any associated tests..."]

sampleTime :: UTCTime
sampleTime = UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 2)
