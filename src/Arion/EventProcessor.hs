{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent, respondToEvent
) where

import           Arion.Types
import           Control.Applicative       ((<$>))
import           Data.List                 (isSuffixOf)
import           Data.List                 (nub)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encodeString)
import           Prelude                   hiding (FilePath)
import           System.FSNotify           (Event (..))



respondToEvent (Modified filePath time) = Just (filePath,time)
respondToEvent (Added filePath time) = Just (filePath,time)
respondToEvent _ = Nothing

-- commands :: SourceTestMap -> String -> String -> (FilePath,a) -> [Command]
processEvent :: M.Map String [TestFile] -> String -> String -> (FilePath, t) -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder (filePath,_)
        | isSuffixOf "hs" encodedFilePath =
          let fileType = typeOf encodedFilePath
              commandCandidates = case fileType of
                Source -> nub . map testFilePath . fromMaybe []
                          $ M.lookup encodedFilePath sourceToTestFileMap
                Test ->   [encodedFilePath]
          in Echo (encodedFilePath ++ " changed") :
             map (RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []
  where encodedFilePath = encodeString filePath
