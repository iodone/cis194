module Homework10.AParserSpec where

    import Homework10.AParser
    import Test.Hspec

    spec :: Spec
    spec = describe "Homework10.Party" $ do
    
        describe "fmap and applicative" $ do
            it "runParser with ParseName" $ do
                runParser parseName "Hello 1234567" `shouldBe` Just ("Hello", "1234567")
                runParser parseName "Hello" `shouldBe` Just ("Hello", "")
                runParser parseName "" `shouldBe` Nothing
            
            it "runParer with ParsePhone" $ do
                runParser parsePhone "Hello 1234567" `shouldBe` Nothing 
                runParser parsePhone "1234567abc" `shouldBe` Just ("1234567", "abc")
                runParser parsePhone "Hello" `shouldBe` Nothing
                runParser parsePhone "" `shouldBe` Nothing

            it "runParer with Parse Employee" $ do
                runParser (Emp <$> parseName <*> parsePhone ) "Hello 1234567" `shouldBe` Just ((Emp "Hello" "1234567"), "")
                 
