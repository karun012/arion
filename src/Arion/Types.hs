module Arion.Types where

data Command = Command { commandString :: String } deriving (Eq, Show)

data SourceFile = SourceFile { sourceFilePath :: String } deriving (Eq, Show, Ord)

data TestFile = TestFile { testFilePath :: String } deriving (Eq, Show, Ord)

data FileType = Source | Test
