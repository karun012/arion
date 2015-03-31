module Arion.UtilitiesSpec where

import           Arion.Types
import           Arion.Utilities
import           Data.Map
import           Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "associate source and test files" $ do
        it "can find transitive dependencies" $ do
            let sourceFile1 = SourceFile { sourceFilePath = "mydir/ModuleA.hs", moduleName = "ModuleA", importedModules = ["ModuleB"] }
            let sourceFile2 = SourceFile { sourceFilePath = "mydir/ModuleB.hs", moduleName = "ModuleB", importedModules = ["ModuleC"] }
            let sourceFile3 = SourceFile { sourceFilePath = "mydir/ModuleC.hs", moduleName = "ModuleC", importedModules = [] }

            dependencies [] `shouldBe` []
            dependencies [sourceFile1, sourceFile2, sourceFile3] `shouldBe` [(sourceFile1, []),
                                                                             (sourceFile2, [sourceFile1]),
                                                                             (sourceFile3, [sourceFile2, sourceFile1])]
        it "does not break because of cyclic dependencies" $ do
            let sourceFile1 = SourceFile { sourceFilePath = "mydir/ModuleA.hs", moduleName = "ModuleA", importedModules = ["ModuleB"] }
            let sourceFile2 = SourceFile { sourceFilePath = "mydir/ModuleB.hs", moduleName = "ModuleB", importedModules = ["ModuleA"] }
            dependencies [sourceFile1, sourceFile2] `shouldBe` [(sourceFile1, [sourceFile2]),
                                                                (sourceFile2, [sourceFile1])]
        it "finds test files associated with source files and makes a map out of them" $ do
            let sourceFile1 = SourceFile { sourceFilePath = "mydir/ModuleA.hs", moduleName = "ModuleA", importedModules = [] }
            let sourceFile2 = SourceFile { sourceFilePath = "mydir/ModuleB.hs", moduleName = "ModuleB", importedModules = [] }
            let sourceFile3 = SourceFile { sourceFilePath = "mydir/ModuleC.hs", moduleName = "ModuleC", importedModules = [] }
            let testFile1 = TestFile { testFilePath = "mytestdir/ModuleASpec.hs", imports = ["ModuleA", "ModuleB"] }
            let testFile2 = TestFile { testFilePath = "mytestdir/ModuleBSpec.hs", imports = ["ModuleB", "ModuleC"] }
            let testFile3 = TestFile { testFilePath = "mytestdir/ModuleCSpec.hs", imports = ["ModuleA", "ModuleC"] }
            let expected = fromList [("mydir/ModuleA.hs", [testFile1, testFile3]),
                                     ("mydir/ModuleB.hs", [testFile1, testFile2]),
                                     ("mydir/ModuleC.hs", [testFile2, testFile3])]
            let sourceFiles = [sourceFile1, sourceFile2, sourceFile3]
            let testFiles = [testFile1, testFile2, testFile3]
            associate sourceFiles testFiles `shouldBe` expected
        it "considers transitive dependencies" $ do
            let sourceFile1 = SourceFile { sourceFilePath = "mydir/ModuleA.hs", moduleName = "ModuleA", importedModules = ["ModuleB"] }
            let sourceFile2 = SourceFile { sourceFilePath = "mydir/ModuleB.hs", moduleName = "ModuleB", importedModules = ["ModuleC"] }
            let sourceFile3 = SourceFile { sourceFilePath = "mydir/ModuleC.hs", moduleName = "ModuleC", importedModules = [] }
            let testFile1 = TestFile { testFilePath = "mytestdir/ModuleASpec.hs", imports = ["ModuleA"] }
            let testFile2 = TestFile { testFilePath = "mytestdir/ModuleBSpec.hs", imports = ["ModuleB"] }
            let testFile3 = TestFile { testFilePath = "mytestdir/ModuleCSpec.hs", imports = ["ModuleC"] }
            let expected = fromList [("mydir/ModuleA.hs", [testFile1]),
                                     ("mydir/ModuleB.hs", [testFile2, testFile1]),
                                     ("mydir/ModuleC.hs", [testFile3, testFile2, testFile1])]
            let sourceFiles = [sourceFile1, sourceFile2, sourceFile3]
            let testFiles = [testFile1, testFile2, testFile3]
            associate sourceFiles testFiles `shouldBe` expected
