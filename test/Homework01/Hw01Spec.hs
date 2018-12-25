{-# OPTIONS_GHC -Wall #-}
module Homework01.Hw01Spec where

import Homework01.Hw01
import Test.Hspec

spec :: Spec
spec = describe "homework01.Hw01" $ do

  describe "toDigitsRev" $ do
    it "converts positive Integers to a reversed list of digits" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

    it "returns the empty list for 0 input" $ do
      toDigitsRev 0 `shouldBe` []

    it "returns the empty list for negative inputs" $ do
      toDigitsRev (-17) `shouldBe` []

  describe "toDigits" $ do
    it "converts positive Integers to a list of digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]

    it "returns the empty list for 0 input" $ do
      toDigits 0 `shouldBe` []

    it "returns the empty list for negative inputs" $ do
      toDigits (-17) `shouldBe` []