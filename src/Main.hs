module Main where

import System.Environment
import Arion.Runner

main :: IO ()
main = getArgs >>= run
