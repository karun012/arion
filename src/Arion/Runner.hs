module Arion.Runner(
    run
) where

import System.FSNotify (watchTree, withManager, WatchManager, Event)
import Data.Text (pack)
import Filesystem.Path.CurrentOS (fromText)
import System.Process (callCommand)
import Control.Monad (mapM_)
import System.FilePath.Find (find, always, extension, (==?), (||?))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Exception (try, SomeException)
import Control.Concurrent (forkIO)
import System.Directory (canonicalizePath)
import Control.Applicative ((<$>), liftA2, (<*>))
import Control.Monad ((=<<), liftM2)

import Arion.Types
import Arion.EventProcessor
import Arion.Utilities
import Arion.Help

run :: [String] -> IO ()
run args
    | "--help" `elem` args = putStrLn usage
    | length args >= 3 = let (path:sourceFolder:testFolder:_) = args
                         in withManager (startWatching path sourceFolder testFolder)
    | otherwise = putStrLn "Try arion --help for more information"

startWatching :: String -> String -> String -> WatchManager -> IO a
startWatching path sourceFolder testFolder manager = let sourceFiles = map (uncurry toSourceFile) <$> (mapM filePathAndContent =<< findHaskellFiles sourceFolder)
                                                         testFiles = map (uncurry toTestFile) <$> (mapM filePathAndContent =<< findHaskellFiles testFolder)
                                                         sourceToTestFileMap = associate <$> sourceFiles <*> testFiles
                                                         watchTreeWithHandler = watchTree manager (fromText $ pack path) (const True)
                                                     in (watchTreeWithHandler =<< (eventHandler <$> sourceToTestFileMap)) >> (forever $ threadDelay maxBound)

filePathAndContent :: String -> IO (FilePath, FileContent)
filePathAndContent relativePath = let canonicalizedPath = canonicalizePath relativePath
                                      content = readFile =<< canonicalizedPath
                                  in liftM2 (,) canonicalizedPath content

findHaskellFiles :: String -> IO [String]
findHaskellFiles = find always (extension ==? ".hs" ||? extension ==? ".lhs")

eventHandler :: SourceTestMap -> Event -> IO ()
eventHandler sourceToTestFileMap event = let commands = processEvent sourceToTestFileMap event
                                         in mapM_ executeCommand commands

executeCommand :: Command -> IO ()
executeCommand command = let process = (try . callCommand) (show command) :: IO (Either SomeException ())
                         in void $ forkIO $ process >> return ()
