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
      
  describe "doubleEveryOther" $ do
    it "empty list for return []" $ do
      doubleEveryOther [] `shouldBe` []
    it "only one elem in list" $ do
      doubleEveryOther [3] `shouldBe` [3]
    it "double the list length of even" $ do
      doubleEveryOther [1,2,3,4] `shouldBe` [2,2,6,4]
    it "double the list length of odd" $ do
      doubleEveryOther [4,7,9] `shouldBe` [4,14,9]

  describe "sumDigits" $ do
    it "empty list or negiva list for return 0 " $ do
      sumDigits [] `shouldBe` 0
      sumDigits [-1, -3, -9] `shouldBe` 0
    it "only one-digit list return" $ do
      sumDigits [1, 3, 5, 9] `shouldBe` 18
    it "hybrid one-digit and two-digit list" $ do
      sumDigits [16,7,12,5] `shouldBe` 22

  describe "validate" $ do
    it "validate credit card number" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False

  describe "hanoi move" $ do

    let a = "a"
    let b = "b"
    let c = "c"

    it "solves the problem for 1 disc" $ do
      hanoi 1 a b c `shouldBe` [(a,b)]

    it "solves the problem for 2 discs" $ do
      hanoi 2 a b c `shouldBe` [(a,c), (a,b), (c,b)]

    it "solves the problem for 3 discs" $ do
      hanoi 3 a b c `shouldBe` [(a,b), (a,c), (b,c), (a,b), (c,a), (c,b), (a,b)]
