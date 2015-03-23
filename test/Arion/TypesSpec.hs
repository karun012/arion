module Arion.TypesSpec where

import Test.Hspec
import Arion.Types

main :: IO ()
main = hspec spec

spec = do
    describe "Types" $ do
        it "can convert a file into a SourceFile" $ do
            let filePath = "mydir/Source.hs"
            let content = "module Source where"
            let expected = SourceFile { 
                                sourceFilePath = "mydir/Source.hs",
                                moduleName = "Source"
                           }
            toSourceFile filePath content `shouldBe` expected
        it "can convert a file into a TestFile" $ do
            let filePath = "mytestdir/SampleSpec.hs"
            let content = "module SampleSpec where\n\
                          \import Module1\n\
                          \import Another.Module\n\
                          \import Yet.Another.Module\n"
            let expected = TestFile { 
                                testFilePath = "mytestdir/SampleSpec.hs",
                                imports = ["Module1", "Another.Module", "Yet.Another.Module"]
                           }
            toTestFile filePath content `shouldBe` expected

