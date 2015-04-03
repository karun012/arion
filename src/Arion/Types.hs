module Arion.Types (
    Command(..),
    FileContent,
    SourceTestMap,
    SourceFile(..),
    TestFile(..),
    FileType(..),
    toSourceFile,
    toTestFile,
    typeOf
) where

import           Data.List        (isInfixOf)
import           Data.List.Split  (splitOn)
import           Data.Map         (Map)
import           Data.Maybe       (mapMaybe)
import           Text.Regex.Posix (getAllTextMatches, (=~))

data Command = RunHaskell { sourceFolder :: String, testFolder :: String, commandString :: String } |
               Echo String
                deriving (Eq,Ord)

instance Show Command where
    show (RunHaskell sourceFolder testFolder commandString) = "cabal exec runhaskell -- -i" ++ sourceFolder ++ " -i" ++ testFolder ++ " " ++ commandString
    show (Echo stringToEcho) = "echo " ++ stringToEcho


type FileContent = String

type SourceTestMap = Map FilePath [TestFile]

data SourceFile = SourceFile {
    sourceFilePath  :: String,
    moduleName      :: String,
    importedModules :: [String]
} deriving (Eq, Ord, Show)

data TestFile = TestFile {
    testFilePath :: String,
    imports      :: [String]
} deriving (Eq, Ord, Show)

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
getImports fileContent = let importLines = getAllTextMatches $ fileContent =~ "^import .*" :: [String]
                             imports = mapMaybe (getSecond . filter (not . (`elem` ["","qualified"])) . splitOn " ") importLines
                         in imports


getSecond (_:x:_) = Just x
getSecond _ = Nothing

-- this is just wrong - we can't always find a module in a file.
-- need to rework to propagate Maybe up through toSourceFile etc.
getModuleName :: FileContent -> String
getModuleName fileContent = let moduleLine = fileContent =~ "(module\\s+.*\\s+.*)" :: String
                                (_:moduleName:_)= splitOn " " moduleLine
                            in moduleName
