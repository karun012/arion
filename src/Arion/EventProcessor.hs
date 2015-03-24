{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Arion.Help

processEvent :: SourceTestMap -> Event -> [Command]
processEvent sourceToTestFileMap (Modified filePath _) = commands sourceToTestFileMap (encodeString filePath)
processEvent sourceToTestFileMap (Added filePath _) = commands sourceToTestFileMap (encodeString filePath)
processEvent _ _ = []

commands :: SourceTestMap -> FilePath -> [Command]
commands sourceToTestFileMap filePath 
        | isSuffixOf "hs" filePath = let fileType = typeOf filePath
                                         commandCandidates = case fileType of
                                                               Source -> let testFiles = M.lookup filePath sourceToTestFileMap
                                                                         in toCommandCandidates testFiles
                                                               Test -> [filePath]
                                     in Echo (filePath ++ " changed") : map RunHaskell commandCandidates
        | otherwise = []

toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

