module Arion.TypesSpec where

import Test.Hspec
import Arion.Types

main :: IO ()
main = hspec spec

spec = do
    describe "Types" $ do
        it "can make a SourceFile out of source code and path to file" $ do
            let sampleSource = "module MyProject.ModuleA where"
            let sampleSourcePath = "src/ModuleA.hs"
            let expectedSourceFile = SourceFile { sourceFilePath = "src/ModuleA.hs", moduleName = "MyProject.ModuleA" }

            toSourceFile sampleSourcePath sampleSource `shouldBe` expectedSourceFile
        it "can make a TestFile out of test code and path to file" $ do
            let sampleTest = "module MyProject.ModuleASpec where\n\
                             \import MyProject.ModuleA\n\n\
                             \import MyProject.SomethingElse.ModuleB"
            let sampleTestPath = "test/ModuleASpec.hs"
            let expectedTestFile = TestFile { testFilePath = "test/ModuleASpec.hs", imports = ["MyProject.ModuleA", "MyProject.SomethingElse.ModuleB"] }

            toTestFile sampleTestPath sampleTest `shouldBe` expectedTestFile

