module Homework02.LogAnalysisSpec where

import Homework02.LogAnalysis
import Homework02.Log

import Test.Hspec

smallTree :: MessageTree
smallTree = Node
    (Node Leaf (LogMessage Info 1 "1111") Leaf)
    (LogMessage Info 3 "3333")
    (Node Leaf (LogMessage Info 5 "5555") Leaf)

newTree :: MessageTree
newTree = Node
    (Node Leaf (LogMessage Info 1 "1111") (Node Leaf (LogMessage (Error 100) 2 "2222") Leaf))
    (LogMessage Info 3 "3333")
    (Node (Node Leaf (LogMessage Info 4 "4444") Leaf) (LogMessage Info 5 "5555") Leaf)

logMessages :: [LogMessage]
logMessages = 
    [
    (LogMessage Info 3 "3333"),
    (LogMessage Info 1 "1111"),
    (LogMessage Info 5 "5555"),
    (LogMessage (Error 100) 2 "2222"),
    (LogMessage Info 4 "4444")
    ]

sortedLogMessages :: [LogMessage]
sortedLogMessages = 
    [
    (LogMessage Info 1 "1111"),
    (LogMessage (Error 100) 2 "2222"),
    (LogMessage Info 3 "3333"),
    (LogMessage Info 4 "4444"),
    (LogMessage Info 5 "5555")
    ]

postmortemMessages :: [LogMessage]
postmortemMessages =
    [ (LogMessage Info 1 "I1")
    , (LogMessage (Error 40) 2 "E2")
    , (LogMessage (Error 50) 4 "E4")
    , (LogMessage (Error 51) 3 "E3")
    ]

postmortemResult :: [String]
postmortemResult = ["E3", "E4"]

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
    
    describe "insert" $ do
        it "returns unchanged tree when given unknown log message" $ do
            insert (Unknown "") smallTree `shouldBe` smallTree
        it "Insert logMessage to smallTree" $ do
            let a = insert (LogMessage (Error 100) 2 "2222") smallTree
            let b = insert (LogMessage Info 4 "4444") a
            b `shouldBe` newTree

    describe "build" $ do
        it "returns Leaf when []" $ do
            build [] `shouldBe` Leaf
        it "return newTree" $ do
            build logMessages `shouldBe` newTree
    
    describe "inOrder" $ do
        it "return sorted log message when newTree input" $ do
            inOrder newTree `shouldBe` sortedLogMessages
    
    describe "whatWentWrong" $ do
        it "return revelent error message string" $ do
            whatWentWrong postmortemMessages `shouldBe` postmortemResult