module Arion.Runner where

import System.FSNotify
import Data.Maybe (maybe)
import Safe (headMay)
import Data.Text (pack)
import Filesystem.Path.CurrentOS (fromText)
import System.Exit (exitSuccess)
import System.Process (callCommand)
import System.Process.Internals
import System.Exit (ExitCode(..))
import Control.Monad (mapM_)
import Data.Map (empty)
import System.FilePath.Find
import Data.Map (Map)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Exception
import Control.Concurrent
import System.Directory (canonicalizePath)
import Control.Applicative ((<$>), liftA2, (<*>))
import Control.Monad
import Control.Monad.IO.Class

import Arion.Types
import Arion.EventProcessor
import Arion.Utilities
import Arion.Help

filePathFromArgs :: [String] -> String
filePathFromArgs = maybe "." id . headMay

run :: [String] -> IO ()
run args
    | "--help" `elem` args = putStrLn usage
    | length args >= 3 = let (path:sourceFolder:testFolder:_) = args
                         in withManager (startWatching path sourceFolder testFolder)
    | otherwise = putStrLn "Try arion --help for more information"

startWatching :: String -> String -> String -> WatchManager -> IO a
startWatching path sourceFolder testFolder manager = do
                        let sourceFilePathsRelative = find always (extension ==? ".hs" ||? extension ==? ".lhs") sourceFolder
                        let testFilePathsRelative = find always (extension ==? ".hs" ||? extension ==? ".lhs") testFolder
                        let sourceFilePaths = mapM canonicalizePath =<< sourceFilePathsRelative
                        let testFilePaths = mapM canonicalizePath =<< testFilePathsRelative
                        let sourceFileContents = mapM readFile =<< sourceFilePaths
                        let testFileContents = mapM readFile =<< testFilePaths
                        let sourceFileContentZipped = liftA2 zip sourceFilePaths sourceFileContents
                        let testFileContentZipped = liftA2 zip testFilePaths testFileContents
                        let sourceFiles = map (uncurry toSourceFile) <$> sourceFileContentZipped
                        let testFiles = map (uncurry toTestFile) <$> testFileContentZipped
                        sourceToTestFileMap <- associate <$> sourceFiles <*> testFiles

                        _ <- watchTree manager (fromText $ pack path) (const True) (eventHandler sourceToTestFileMap)
                        forever $ threadDelay maxBound

eventHandler :: SourceTestMap -> Event -> IO ()
eventHandler sourceToTestFileMap event = do
                                let commands = processEvent sourceToTestFileMap event
                                mapM_ executeCommand commands

executeCommand :: Command -> IO ()
executeCommand command = void $ forkIO $ do
                            _ <- (try . callCommand) (show command) :: IO (Either SomeException ())
                            return ()
