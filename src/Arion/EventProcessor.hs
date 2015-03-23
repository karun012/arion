{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Map as M
import Data.List (isSuffixOf)

processEvent :: M.Map String [TestFile] -> Event -> [Command]
processEvent sourceToTestFileMap (Modified filePath _) = commands sourceToTestFileMap (encodeString filePath)
processEvent sourceToTestFileMap (Added filePath _) = commands sourceToTestFileMap (encodeString filePath)
processEvent _ _ = []

commands :: M.Map String [TestFile] -> FilePath -> [Command]
commands sourceToTestFileMap filePath 
        | isSuffixOf "hs" filePath = let fileType = typeOf filePath
                                         commandCandidates = case fileType of
                                                               Source -> let testFiles = M.lookup filePath sourceToTestFileMap
                                                                         in toCommandCandidates testFiles
                                                               Test -> [filePath]
                                     in map (Command . (++) "runhaskell -isrc ") commandCandidates
        | otherwise = []

toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

