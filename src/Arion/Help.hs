module Arion.Help (
    usage
) where

usage :: String
usage = unlines ["Usage: arion [folder to watch] [folder with source files] [folder with test files]",
                "\n  --version",
                "\tshows version information"]

