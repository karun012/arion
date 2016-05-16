{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent, respondToEvent
) where

import           Arion.Types
import           Control.Applicative       ((<$>))
import           Data.List                 (isSuffixOf, isInfixOf)
import           Data.List                 (nub)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import           Prelude                   hiding (FilePath)
import           System.FSNotify           (Event (..))

respondToEvent (Modified filePath time) = Just (decodeString filePath, time)
respondToEvent (Added    filePath time) = Just (decodeString filePath, time)
respondToEvent _ = Nothing

processEvent :: M.Map String [TestFile] -> String -> String -> (FilePath, t) -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder (filePath,_)
        | (not . isInfixOf ".#") encodedFilePath && isSuffixOf "hs" encodedFilePath =
          let fileType = typeOf encodedFilePath
              commandCandidates = case fileType of
                Source -> nub . map testFilePath . fromMaybe []
                          $ M.lookup encodedFilePath sourceToTestFileMap
                Test ->   [encodedFilePath]
              maybeLacksTests = if commandCandidates == [] then [Echo (encodedFilePath ++ " does not have any associated tests...")] else []
          in Echo (encodedFilePath ++ " changed") : maybeLacksTests ++
             map (RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []
  where encodedFilePath = encodeString filePath
