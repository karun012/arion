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
import           Data.Text                 (pack)
import           Data.Time.Clock           (UTCTime)
import           Filesystem.Path           (FilePath)
import           Prelude                   hiding (FilePath)
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import           System.FSNotify           (Event (..))

toPath :: String -> FilePath
toPath = fromText . pack

respondToEvent :: Event -> Maybe (FilePath, UTCTime)
respondToEvent (Modified filePath time) = Just (toPath filePath, time)
respondToEvent (Added    filePath time) = Just (toPath filePath, time)
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
