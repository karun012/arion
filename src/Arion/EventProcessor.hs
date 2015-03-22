{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Map as M

processEvent :: M.Map String [TestFile] -> Event -> [Command]
processEvent sourceToTestFileMap (Modified filePath _) = let fileType = typeOf (encodeString filePath)
                                                             commandCandidates = case fileType of
                                                                                   Source -> let testFiles = M.lookup (encodeString filePath) sourceToTestFileMap
                                                                                             in toCommandCandidates testFiles
                                                                                   Test -> [encodeString filePath]
                                                         in map (Command . (++) "runhaskell -isrc ") commandCandidates
processEvent _ _ = []


toCommandCandidates :: Maybe [TestFile] -> [String]
toCommandCandidates (Just testFiles) = map testFilePath testFiles
toCommandCandidates Nothing = [""]

