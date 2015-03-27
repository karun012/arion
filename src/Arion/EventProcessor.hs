{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent
) where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Arion.Help

processEvent :: SourceTestMap -> String -> Event -> [Command]
processEvent sourceToTestFileMap sourceFolder (Modified filePath _) = commands sourceToTestFileMap sourceFolder (encodeString filePath)
processEvent sourceToTestFileMap sourceFolder (Added filePath _) = commands sourceToTestFileMap sourceFolder (encodeString filePath)
processEvent _ _ _ = []

commands :: SourceTestMap -> String -> FilePath -> [Command]
commands sourceToTestFileMap sourceFolder filePath
        | isSuffixOf "hs" filePath = let fileType = typeOf filePath
                                         commandCandidates = case fileType of
                                                               Source -> let testFiles = M.lookup filePath sourceToTestFileMap
                                                                         in toCommandCandidates testFiles
                                                               Test -> [filePath]
                                     in Echo (filePath ++ " changed") : map (CabalCommand . RunHaskell sourceFolder) commandCandidates
        | otherwise = []

toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

