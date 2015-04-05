module Arion.Help (
    usage
) where

usage :: String
usage = unlines ["arion by default assumes that it needs to watch the current folder for changes, and that src and test are the source and test folders.",
                "\nUsage: arion <folder to watch> <folder with source files> <folder with test files>", 
                "\n  --version",
                "\tshows version information"]
