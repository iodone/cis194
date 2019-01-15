module Homework03.GolfSpec where

import Homework03.Golf
import Test.Hspec

histogram1 :: String
histogram1 = unlines
  [ " *        "
  , " *        "
  , " *   *    "
  , "=========="
  , "0123456789" ]

histogram2 :: String
histogram2 = unlines
  [ "    *     "
  , "    *     "
  , "    * *   "
  , " ******  *"
  , "=========="
  , "0123456789" ]


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

    describe "localMaxima" $ do
        it "for boundry case" $ do
            localMaxima [] `shouldBe` []
            localMaxima [3] `shouldBe` []
            localMaxima [3,2] `shouldBe` []
        it "for more normal case"  $ do
            localMaxima [2,9,5,6,1] `shouldBe` [9,6]
            localMaxima [2,3,4,1,5] `shouldBe` [4]
            localMaxima [1,2,3,4,5] `shouldBe` []
    
    describe "histogram" $ do
        it "works for one input" $ do
            histogram [1,1,1,5] `shouldBe` histogram1
    
        it "works for another input" $ do
            histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` histogram2