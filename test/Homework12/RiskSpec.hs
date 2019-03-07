module Homework12.RiskSpec where

    import Homework12.Risk
    import Control.Monad.Random 
    import Test.Hspec

    spec :: Spec
    spec = describe "Homework12.Risk" $ do
    
        describe "getRandom" $ do
            it "die" $ do
                1 `shouldBe` 1