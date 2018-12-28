module Homework02.LogAnalysisSpec where

import Homework02.LogAnalysis
import Homework02.Log

import Test.Hspec

spec :: Spec
spec = describe "Homework02.LogAnalysis" $ do

  describe "parseMessage" $ do
    it "Info log" $ do
        parseMessage "I 6 Completed armadillo processin" `shouldBe` LogMessage Info 6 "Completed armadillo processin"

    it "Warning log" $ do
        parseMessage "W 5 Flange is due for a check-up" `shouldBe` LogMessage Warning 5 "Flange is due for a check-up"
        
    it "Error log" $ do
        parseMessage "E 20 2 Too many pickles" `shouldBe` LogMessage (Error 20) 2 "Too many pickles"

    it "Unknown log" $ do
        parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
        