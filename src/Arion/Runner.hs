module Arion.Runner(
    run
) where

import           Arion.EventProcessor
import           Arion.Help
import           Arion.Types
import           Arion.Utilities
import           Control.Applicative       ((<$>))
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent        (putMVar)
import           Control.Concurrent        (takeMVar)
import           Control.Concurrent        (newEmptyMVar)
import           Control.Concurrent        (MVar)
import           Control.Exception         (SomeException, try)
import           Control.Exception         (bracket_)
import           Control.Monad             (forever, void)
import           Data.IORef                (IORef, atomicModifyIORef', newIORef)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (fromText)
import           System.Directory          (canonicalizePath)
import           System.FilePath.Find      (always, extension, find, (==?),
                                            (||?))
import           System.FSNotify           (WatchManager, watchTree,
                                            withManager)
import           System.Process            (callCommand)

run :: [String] -> IO ()
run args
    | "--help" `elem` args = putStrLn usage
    | length args >= 3 = let (path:sourceFolder:testFolder:_) = args
                         in withManager (startWatching path sourceFolder testFolder)
    | otherwise = putStrLn "Try arion --help for more information"

startWatching :: String -> String -> String -> WatchManager -> IO ()
startWatching path sourceFolder testFolder manager = do
  sourceFiles <- mapM (\x -> uncurry toSourceFile <$> filePathAndContent x)
                 =<< findHaskellFiles sourceFolder
  testFiles   <- mapM (\x -> uncurry toTestFile <$> filePathAndContent x)
                 =<< findHaskellFiles testFolder

  let sourceToTestFileMap = associate sourceFiles testFiles
  lock <- newEmptyMVar
  inProgress <- newIORef Map.empty
  _ <- watchTree manager (fromText $ pack path) (const True)
       (eventHandler lock inProgress (processEvent sourceToTestFileMap sourceFolder testFolder))
  forever $ threadDelay maxBound

filePathAndContent :: String -> IO (FilePath, FileContent)
filePathAndContent relativePath = do
                          canonicalizedPath <- canonicalizePath relativePath
                          content <- readFile canonicalizedPath
                          return (canonicalizedPath, content)

findHaskellFiles :: String -> IO [String]
findHaskellFiles = find always (extension ==? ".hs" ||? extension ==? ".lhs")


-- 10th of a sec? seems ok.
dELAY = 100000

eventHandler :: MVar () -> IORef (Map Command ()) -> (t -> [Command]) -> t -> IO ()
eventHandler lock inProgress handler x =
  mapM_ (executeCommand lock inProgress) $ handler x


executeCommand :: MVar () -> IORef (Map Command ()) -> Command -> IO ()
executeCommand lock inProgress command@(RunHaskell{}) = do
  todo <- atomicModifyIORef' inProgress
          (\running -> case Map.lookup command running of
              Just _ -> (running,return ())
              Nothing -> (Map.insert command () running,
                          do threadDelay dELAY
                             atomicModifyIORef' inProgress
                               (\hash -> (Map.delete command hash,
                                          ()))))
  todo
  runCommand lock command
executeCommand lock _ command = runCommand lock command


runCommand lock command = do
  let process = (try . callCommand) (show command) :: IO (Either SomeException ())
  bracket_ (putMVar lock ())
           (takeMVar lock)
           (void $ process >> return ())
