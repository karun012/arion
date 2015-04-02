{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor (
    processEvent, respondToEvent
) where

import           Arion.Types
import           Control.Applicative       ((<$>))
import           Data.List                 (find, isSuffixOf, nub)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Filesystem.Path           (FilePath)
import           Filesystem.Path.CurrentOS (encodeString)
import           Prelude                   hiding (FilePath)
import           System.FSNotify           (Event (..))



respondToEvent (Modified filePath time) = Just (filePath,time)
respondToEvent (Added filePath time) = Just (filePath,time)
respondToEvent _ = Nothing

processEvent :: M.Map String [TestFile] -> String -> String -> [SourceFile] -> (FilePath, t) -> [Command]
processEvent sourceToTestFileMap sourceFolder testFolder allSources (filePath,_)
        | isSuffixOf "hs" encodedFilePath =
          let fileType = typeOf encodedFilePath
              commandCandidates = case fileType of
                Source -> nub . map testFilePath . fromMaybe []
                          $ M.lookup encodedFilePath sourceToTestFileMap
                Test ->   [encodedFilePath]
              maybeLacksTests = if commandCandidates == [] then [Echo (encodedFilePath ++ " does not have any associated tests...")] else []
              sourceFile = find (\file -> sourceFilePath file == encodedFilePath) allSources
              whatChanged = case sourceFile of
                                Just source -> (Echo $ moduleName source ++ " changed") : [Echo (moduleName source ++ " is associated with these tests")]
                                _ -> [Echo $ encodedFilePath ++ " changed"]
              testFileEchoCommands = if fileType == Test then [] else map Echo commandCandidates
          in whatChanged ++ testFileEchoCommands ++ maybeLacksTests ++
             map (RunHaskell sourceFolder testFolder ) commandCandidates
        | otherwise = []
  where encodedFilePath = encodeString filePath
