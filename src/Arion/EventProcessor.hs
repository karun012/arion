{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent
) where

import Arion.Types
import System.FSNotify (Event(..))
import Filesystem.Path.CurrentOS (encodeString)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Arion.Help

processEvent :: SourceTestMap -> String -> String -> Event -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder (Modified filePath _) = commands sourceToTestFileMap sourceFolder testFolder (encodeString filePath)
processEvent sourceToTestFileMap sourceFolder testFolder (Added filePath _) = commands sourceToTestFileMap sourceFolder testFolder (encodeString filePath)
processEvent _ _ _ _ = []

commands :: SourceTestMap -> String -> String -> FilePath -> [Command]
commands sourceToTestFileMap sourceFolder testFolder filePath
        | isSuffixOf "hs" filePath = let fileType = typeOf filePath
                                         commandCandidates = case fileType of
                                                               Source -> let testFiles = M.lookup filePath sourceToTestFileMap
                                                                         in toCommandCandidates testFiles
                                                               Test -> [filePath]
                                     in Echo (filePath ++ " changed") : map (CabalExec . RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []

toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

