module Arion.Help (
    usage
) where

usage :: String
usage = unlines $ ["Usage: arion <folder to watch> <folder with source files> <folder with test files>",
                   "       arion",
                   "\nIf invoked without argumets '.', 'src' and 'test' are assumed for \"folder to watch\", \"folder with source files\" and \"folder with test files\" respectively.",
                   "\n  --version",
                   "\tshows version information"]
