module Arion.Types where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Text.Regex.Posix

data Command = Command { commandString :: String } deriving (Eq, Show)

type FileContent = String

data SourceFile = SourceFile { 
    sourceFilePath :: String,
    moduleName :: String
} deriving (Eq, Show, Ord)

data TestFile = TestFile { 
    testFilePath :: String,
    imports :: [String]
} deriving (Eq, Show, Ord)

data FileType = Source | Test

typeOf :: String -> FileType
typeOf filePath
       | isInfixOf "Spec" filePath == True = Test
       | otherwise = Source
                   
toSourceFile :: FilePath -> FileContent -> SourceFile
toSourceFile filePath content = let theModuleName = getModuleName content
                                in SourceFile {
                                      sourceFilePath = filePath,
                                      moduleName = theModuleName
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
