{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Arion.Runner(
    run
) where

import           Paths_arion               (version)

import           Arion.EventProcessor
import           Arion.Help
import           Arion.Types
import           Arion.Utilities
import           Control.Applicative       ((<$>))
import           Control.Concurrent        (MVar, newEmptyMVar, putMVar,
                                            takeMVar, threadDelay)
import           Control.Exception         (SomeException, bracket_, try)
import           Control.Monad             (forever, void)
import           Control.Monad             (join)
import           Data.IORef                (IORef, atomicModifyIORef', newIORef)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (pack)
import           Data.Version              (showVersion)
import           Filesystem.Path.CurrentOS (fromText)
import           System.IO
import           System.Exit
import           System.Directory          (canonicalizePath)
import           System.FilePath.Find      (always, extension, find, (==?),
                                            (||?))
import           System.FSNotify           (WatchManager, watchTree,
                                            withManager)
import           System.Process            (callCommand)


run :: [String] -> IO ()
run args
    | "--help" `elem` args = putStrLn usage
    | "--version" `elem` args = putStrLn ("Arion version " ++ showVersion version)
    | otherwise = case args of
        [] -> withManager (startWatching "." "src" "test")
        path:sourceFolder:testFolder:[] -> withManager (startWatching path sourceFolder testFolder)
        _ -> hPutStrLn stderr "Try arion --help for more information" >> exitFailure

startWatching :: String -> String -> String -> WatchManager -> IO ()
startWatching path sourceFolder testFolder manager = do
  putStrLn "watching ."
  putStrLn "looking for source files in src/"
  putStrLn "looking for test files in test/"

  sourceFiles <- mapM (\x -> uncurry toSourceFile <$> filePathAndContent x)
                 =<< findHaskellFiles sourceFolder
  testFiles   <- mapM (\x -> uncurry toTestFile <$> filePathAndContent x)
                 =<< findHaskellFiles testFolder

  let sourceToTestFileMap = associate sourceFiles testFiles

  lock <- newEmptyMVar
  inProgress <- newIORef Map.empty
  _ <- watchTree manager (fromText $ pack path) (const True)
       (eventHandler lock inProgress (processEvent sourceToTestFileMap sourceFolder testFolder) . respondToEvent)
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

eventHandler lock inProgress handler Nothing  = return ()
eventHandler lock inProgress handler (Just (filePath, time)) = do
  commands <- join $ atomicModifyIORef' inProgress
          (\running -> case Map.lookup filePath running of
              Just _ -> (running,return [])
              Nothing -> (Map.insert filePath () running,
                          do threadDelay dELAY
                             atomicModifyIORef' inProgress
                               (\hash -> (Map.delete filePath hash,
                                          ()))
                             return $ handler (filePath, time)
                         ))

  mapM_ (runCommand lock) commands

runCommand :: Show a => MVar () -> a -> IO ()
runCommand lock command = do
  bracket_ (putMVar lock ())
           (takeMVar lock)
           (noisyTry (callCommand $ show command))

noisyTry :: IO () -> IO ()
noisyTry function = do
  (exception :: Either SomeException ()) <- try function
  case exception of
    Left ex -> print ("error", ex)
    _ ->  return ()
