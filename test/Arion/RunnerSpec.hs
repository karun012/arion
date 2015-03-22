module Arion.RunnerSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "Runner" $ do
        it "todo" $ do
            True `shouldBe` True
