module Arion.TypesSpec where

import Test.Hspec
import Arion.Types

main :: IO ()
main = hspec spec

spec = do
    describe "Types" $ do
        it "can make a SourceFile out of source code and path to file" $ do
            let sampleSource = "module ModuleA where"
            let sampleSourcePath = "src/ModuleA.hs"
            let expectedSourceFile = SourceFile { sourceFilePath = "src/ModuleA.hs", moduleName = "ModuleA" }

            toSourceFile sampleSourcePath sampleSource `shouldBe` expectedSourceFile
        it "can make a TestFile out of test code and path to file" $ do
            let sampleTest = "module ModuleASpec where\n\
                             \import ModuleA\n\
                             \import ModuleB"
            let sampleTestPath = "test/ModuleASpec.hs"
            let expectedTestFile = TestFile { testFilePath = "test/ModuleASpec.hs", imports = ["ModuleA", "ModuleB"] }

            toTestFile sampleTestPath sampleTest `shouldBe` expectedTestFile
