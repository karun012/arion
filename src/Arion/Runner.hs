{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Arion.Runner(
    run
) where

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
eventHandler lock inProgress handler (Just (fp,time)) = do
  commands <- join $ atomicModifyIORef' inProgress
          (\running -> case Map.lookup fp running of
              Just _ -> (running,return [])
              Nothing -> (Map.insert fp () running,
                          do threadDelay dELAY
                             atomicModifyIORef' inProgress
                               (\hash -> (Map.delete fp hash,
                                          ()))
                             return $ handler (fp,time)
                         ))

  mapM_ (runCommand lock) commands

runCommand :: Show a => MVar () -> a -> IO ()
runCommand lock command = do
  bracket_ (putMVar lock ())
           (takeMVar lock)
           (noisyTry (callCommand $ show command))

noisyTry :: IO () -> IO ()
noisyTry f = do
  (x :: Either SomeException ()) <- try f
  case x of
    Left y -> print ("error", y)
    _ ->  return ()
