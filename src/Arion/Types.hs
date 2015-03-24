module Arion.Types where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Text.Regex.Posix
import Data.Map (Map)

data Command = RunHaskell { commandString :: String } | Echo String deriving (Eq)

instance Show Command where
    show (RunHaskell commandString) = "runhaskell -isrc " ++ commandString
    show (Echo stringToEcho) = "echo " ++ stringToEcho

type FileContent = String

type SourceTestMap = Map FilePath [TestFile]

data SourceFile = SourceFile { 
    sourceFilePath :: String,
    moduleName :: String,
    importedModules :: [String]
} deriving (Eq, Ord)

data TestFile = TestFile { 
    testFilePath :: String,
    imports :: [String]
} deriving (Eq, Ord)

instance Show SourceFile where
    show sourceFile = moduleName sourceFile

instance Show TestFile where
    show testFile = testFilePath testFile

data FileType = Source | Test

typeOf :: String -> FileType
typeOf filePath
       | isInfixOf "Spec" filePath == True = Test
       | otherwise = Source

toSourceFile :: FilePath -> FileContent -> SourceFile
toSourceFile filePath content = let theModuleName = getModuleName content
                                    theImportedModules = getImports content
                                in SourceFile {
                                      sourceFilePath = filePath,
                                      moduleName = theModuleName,
                                      importedModules = theImportedModules
                                   }

toTestFile :: FilePath -> FileContent -> TestFile
toTestFile filePath content = let importLines = getImports content
                                  in TestFile {
                                      testFilePath = filePath,
                                      imports = importLines
                                  }

getImports :: FileContent -> [String]
getImports fileContent = let importLines = getAllTextMatches $ fileContent =~ "import.*" :: [String]
                             imports = map (importedModule . splitOn " ") importLines
                         in imports

importedModule :: [String] -> String
importedModule [_, moduleName] = moduleName
importedModule _ = ""

getModuleName :: FileContent -> String
getModuleName fileContent = let moduleLine = fileContent =~ "(module\\s+.*\\s+where)" :: String
                                [_, moduleName, _] = splitOn " " moduleLine
                            in moduleName
