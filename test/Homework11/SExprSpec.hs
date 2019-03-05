module Homework11.SExprSpec where

    import Homework11.SExpr
    import Homework11.AParser
    import Data.Char
    import Test.Hspec

    spec :: Spec
    spec = describe "Homework11.SExprSpec" $ do
    
        describe "zeroOrMore and oneOrMore" $ do
            it "zeroOrMore" $ do
                runParser (zeroOrMore (satisfy isUpper)) "ABCDgHijk" `shouldBe` Just ("ABCD", "gHijk")
                runParser (zeroOrMore (satisfy isUpper)) "abcdGHijk" `shouldBe` Just ("", "abcdGHijk")
            
            it "oneOrMore" $ do
                runParser (oneOrMore (satisfy isUpper)) "ABCDgHijk" `shouldBe` Just ("ABCD", "gHijk")
                runParser (oneOrMore (satisfy isUpper)) "abcdGHijk" `shouldBe` Nothing 

        describe "spaces and ident" $ do
            it "spaces" $ do
                runParser spaces "  abc"  `shouldBe` Just ("  ", "abc")
                runParser spaces "abc"  `shouldBe` Just ("", "abc")
                runParser spaces ""  `shouldBe` Just ("", "")

            it "ident" $ do
                runParser ident "foobar baz" `shouldBe` Just("foobar", " baz")
                runParser ident "foo33fA" `shouldBe` Just("foo33fA", "")
                runParser ident "2bad" `shouldBe` Nothing 
                runParser ident "" `shouldBe` Nothing 
        
        describe "parseSExpr" $ do
            it "parseSExpr" $ do
                runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
                runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
                runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")
                runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` Just (Comb [Comb [Comb [A (I "lambda"), A (I "x"), Comb [A (I "lambda"), A (I "y"), Comb [A (I "plus"), A (I "x"), A (I "y")]]], A (N 3)], A (N 5)], "")
                runParser parseSExpr "(      lots    of      (    spaces      in    )    this  (  one  )  )" `shouldBe` Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]], "") 