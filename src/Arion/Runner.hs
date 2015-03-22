module Arion.Runner where

import System.FSNotify
import Data.Maybe (maybe)
import Safe (headMay)
import Data.Text (pack)
import Filesystem.Path.CurrentOS (fromText)
import System.Exit (exitSuccess)
import System.Cmd (system)
import System.Exit (ExitCode(..))
import Control.Monad (mapM_)
import Data.Map (empty)

import Arion.Types
import Arion.EventProcessor

filePathFromArgs :: [String] -> String
filePathFromArgs = maybe "." id . headMay

run :: [String] -> IO ()
run args = let path = filePathFromArgs args
           in watchPath path

runCommand :: Command -> IO ExitCode
runCommand (Command commandString) = system commandString
        
watchPath :: FilePath -> IO ()
watchPath path = withManager $ \manager -> do
                       _ <- watchTree manager (fromText $ pack path) (const True) eventHandler
                       _ <- getLine
                       exitSuccess

eventHandler :: Event -> IO ()
eventHandler event = let commands = processEvent empty event
                     in mapM_ runCommand commands
