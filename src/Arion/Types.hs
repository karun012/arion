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

import           Data.List               (isInfixOf)
import           Data.List.Split         (splitOn)
import           Data.Map                (Map)
import           Data.Maybe              (mapMaybe)
import           Language.Haskell.Parser (ParseResult (..), parseModule)
import           Language.Haskell.Syntax (HsModule (..), Module (..),
                                          importModule)

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
toSourceFile filePath content = let parseResult = parseModule content
                                    theModuleName = getModuleName parseResult
                                    theImportedModules = getImports parseResult
                                in SourceFile {
                                      sourceFilePath = filePath,
                                      moduleName = theModuleName,
                                      importedModules = theImportedModules
                                   }

toTestFile :: FilePath -> FileContent -> TestFile
toTestFile filePath content = let parseResult = parseModule content
                                  importLines = getImports parseResult
                                  in TestFile {
                                      testFilePath = filePath,
                                      imports = importLines
                                  }

getImports :: ParseResult HsModule -> [String]
getImports parseResult = case parseResult of
                             ParseOk parsed -> imports parsed
                             _ -> []
                         where imports :: HsModule -> [String]
                               imports (HsModule _ _ _ importDeclrations _) = map ((\(Module name) -> name) . importModule) importDeclrations

getSecond (_:x:_) = Just x
getSecond _ = Nothing

getModuleName :: ParseResult HsModule -> String
getModuleName parseResult = case parseResult of
                                ParseOk parsed -> moduleNameFrom parsed
                                _ -> ""
                            where moduleNameFrom :: HsModule -> String
                                  moduleNameFrom (HsModule _ theModule _ _ _) = (\(Module name) -> name) theModule
