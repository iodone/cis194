module Homework04.Hw04Spec where

import Homework04.Hw04
import Test.Hspec


spec :: Spec
spec = describe "Homework04.Hw04" $ do

    describe "fun1'" $ do
        it "fun1' and fun1 are equivalence" $ do
            fun1 [1,4,5,6,7,8] `shouldBe` (fun1'  [1,4,5,6,7,8])
            fun1 [3,5,6,9,9] `shouldBe` (fun1' [3,5,6,9,9])
            fun1 [] `shouldBe` (fun1' [])

        it "fun2' and fun2 are equivalence" $ do
            fun2 19 `shouldBe` (fun2' 19)
            fun2 1 `shouldBe` (fun2' 1)
            fun2 32 `shouldBe` (fun2' 32)
    