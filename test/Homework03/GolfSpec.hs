module Homework03.GolfSpec where

import Homework03.Golf
import Test.Hspec
    

spec :: Spec
spec = describe "homework03.Golf" $ do

    describe "skips" $ do
        it "[] should return []" $ do
            skips [] `shouldBe` ([] :: [[Integer]])
        it "more normal case" $ do
            skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
            skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
            skips [1] `shouldBe` ([[1]] :: [[Integer]])
            skips [True,False] `shouldBe` [[True,False], [False]]