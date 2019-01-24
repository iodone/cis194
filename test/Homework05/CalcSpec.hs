module Homework05.CalcSpec where

    import Homework05.Calc
    import Homework05.ExprT
    -- import Homework05.Parser
    -- import Homework05.StackVM
    import Test.Hspec
    
    
    spec :: Spec
    spec = describe "Homework05.Calc" $ do
    
        describe "eval" $ do
            it "Lit test" $ do
                eval (Lit 1) `shouldBe` 1
                eval (Lit 2) `shouldBe` 2
                eval (Lit 0) `shouldBe` 0
    
            it "Lit and Add" $ do
                eval  (Add (Lit 1) (Lit 3)) `shouldBe` 4
                eval  (Add (Lit 1) (Lit 0)) `shouldBe` 1
                eval  (Add (Lit 3) (Add (Lit 1) (Lit 0))) `shouldBe` 4
                eval  (Add (Add (Lit 4) (Lit 6)) (Add (Lit 1) (Lit 0))) `shouldBe` 11
            
            it "Lit Add Mul" $ do
                eval  (Mul (Lit 3) (Lit 5)) `shouldBe` 15
                eval  (Mul (Mul (Lit 5) (Lit 4)) (Lit 5)) `shouldBe` 100
                eval  (Mul (Add (Lit 5) (Lit 4)) (Lit 5)) `shouldBe` 45 
    
        describe "evalStr" $ do
            it "Illegal expression evaluate" $ do
                evalStr "fdsab" `shouldBe` Nothing
                evalStr "1*2+" `shouldBe` Nothing
                evalStr "2+1*" `shouldBe` Nothing
                evalStr "+2+1" `shouldBe` Nothing
            it "Valid expression" $ do
                evalStr "(2+3)*4" `shouldBe` Just 20
                evalStr "1+5*6" `shouldBe` Just 31
        
        describe "withVars" $ do
            it "invalid case" $ do
                withVars  [("x",  6)] (add  (lit  3)  (var  "y")) `shouldBe` Nothing
            it "add case"  $ do
                withVars  [("x",  6)]  (add  (lit  3)  (var  "x")) `shouldBe` Just 9
                withVars  [("x",  6)]  (add  (lit  3)  (lit 6)) `shouldBe` Just 9
                withVars  [("x",  6), ("y", 3)]  (add  (var "y")  (var "x")) `shouldBe` Just 9
            it "var case" $ do
                withVars  [("x",  6)]  (var  "x") `shouldBe` Just 6
            it "mul case" $ do
                withVars  [("x",  6)]  (mul (lit  3)  (var  "x")) `shouldBe` Just 18 
                withVars  [("x",  6)]  (mul (lit  3)  (lit 6)) `shouldBe` Just 18
                withVars  [("x",  6), ("y", 3)]  (mul  (var "y")  (var "x")) `shouldBe` Just 18
            it "add and mul case" $ do
                withVars  [("x",  6),  ("y",  3)] (mul (var  "x")  (add  (var  "y")  (var  "x"))) `shouldBe` Just 54
                