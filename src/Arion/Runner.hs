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

import Arion.Types
import Arion.EventProcessor
import Arion.FileUtils

filePathFromArgs :: [String] -> String
filePathFromArgs = maybe "." id . headMay

run :: [String] -> IO ()
run args = withManager $ \manager -> do
                let path = filePathFromArgs args
                sourceFilePaths <- find always (extension ==? ".hs") "src"
                testFilePaths <- find always (extension ==? ".hs") "test"
                sourceFileContents <- mapM readFile sourceFilePaths
                testFileContents <- mapM readFile testFilePaths
                let sourceFiles = map (uncurry toSourceFile) (zip sourceFilePaths sourceFileContents)
                let testFiles = map (uncurry toTestFile) (zip testFilePaths testFileContents)
                let sourceToTestFileMap = associate sourceFiles testFiles
                _ <- watchTree manager (fromText $ pack path) (const True) (eventHandler sourceToTestFileMap)
                forever $ threadDelay maxBound

eventHandler :: Map FilePath [TestFile] -> Event -> IO ()
eventHandler sourceToTestFileMap event = do
                                let commands = processEvent sourceToTestFileMap event
                                mapM_ executeCommand commands

executeCommand :: Command -> IO ()
executeCommand command = void $ forkIO $ do
                            _ <- (try . callCommand) (show command) :: IO (Either SomeException ())
                            return ()
