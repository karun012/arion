{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Map as M
import Data.List (isInfixOf)

processEvent :: M.Map SourceFile [TestFile] -> Event -> [Command]
processEvent sourceToTestFileMap (Modified filePath _) = let fileType = typeOf (encodeString filePath)
                                                             commandCandidates = case fileType of
                                                                                   Source -> let testFiles = M.lookup (SourceFile $ encodeString filePath) sourceToTestFileMap
                                                                                             in toCommandCandidates testFiles
                                                                                   Test -> [encodeString filePath]
                                                         in map (Command . (++) "runhaskell ") commandCandidates

typeOf :: String -> FileType
typeOf filePath
       | isInfixOf "Spec" filePath == True = Test
       | otherwise = Source
                   

toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

