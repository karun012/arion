{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent
) where

import           Arion.Types
import           Control.Applicative       ((<$>))
import           Data.List                 (isSuffixOf)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Filesystem.Path.CurrentOS (encodeString)
import           System.FSNotify           (Event (..))


processEvent :: M.Map String [TestFile] -> String -> String -> Event -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder event =
  fromMaybe [] $ commands sourceToTestFileMap sourceFolder testFolder <$> respondToEvent event


respondToEvent (Modified filePath time) = Just (filePath,time)
respondToEvent (Added filePath time) = Just (filePath,time)
respondToEvent _ = Nothing

-- commands :: SourceTestMap -> String -> String -> (FilePath,a) -> [Command]
commands sourceToTestFileMap sourceFolder testFolder (filePath,_)
        | isSuffixOf "hs" encodedFilePath =
          let fileType = typeOf encodedFilePath
              commandCandidates = case fileType of
                Source -> map testFilePath . fromMaybe []
                          $ M.lookup encodedFilePath sourceToTestFileMap
                Test ->   [encodedFilePath]
          in Echo (encodedFilePath ++ " changed") :
             map (RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []
  where encodedFilePath = encodeString filePath
