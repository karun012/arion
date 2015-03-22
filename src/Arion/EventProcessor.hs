{-# LANGUAGE OverloadedStrings #-}
module Arion.EventProcessor where

import Arion.Types
import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString)

processEvent :: Event -> Command
processEvent (Modified filePath _) = Command $ "runhaskell " ++ encodeString filePath
