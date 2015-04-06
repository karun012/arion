module Arion.HelpSpec where

import Test.Hspec
import Arion.Help

main :: IO ()
main = hspec spec

spec = do
    describe "Help" $ do
        it "can show how to use arion" $ do
            usage `shouldBe` unlines ["Usage: arion [folder to watch] [folder with source files] [folder with test files]", 
                                      "\n  --version",
                                      "\tshows version information"]
