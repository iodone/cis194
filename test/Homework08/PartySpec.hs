module Homework08.PartySpec where

    import Homework08.Employee
    import Homework08.Party
    import Test.Hspec

    guestList0 :: GuestList 
    guestList0 = GL [(Emp "Stan" 1), (Emp "Bob" 2)] 3

    guestList1 :: GuestList 
    guestList1 = GL [(Emp "Stan" 1), (Emp "Bob" 2), (Emp "John" 3)] 6

    guestList2 :: GuestList 
    guestList2 = GL [(Emp "Stan" 1)] 1

    spec :: Spec
    spec = describe "Homework08.Party" $ do
    
        describe "glCons" $ do
            it "insert employee " $ do
                glCons (Emp "John" 3) guestList0  `shouldBe`   GL [(Emp "John" 3), (Emp "Stan" 1), (Emp "Bob" 2)] 6
        
        -- describe "GuestList Monoid" $ do
        --     it "zero element append non-zero element" $ do
        --         (guestList0 <> mempty) == (mempty <> guestList0) `shouldBe` True
        --     it "Associative property test" $ do
        --         (guestList0 <> guestList1) <> guestList2 == guestList0 <> (guestList1 <> guestList2) `shouldBe` True

        describe "moreFun" $ do
            it "guestList1 should greater than guestList0" $ do
                moreFun guestList0 guestList1 `shouldBe` guestList1
                moreFun guestList1 guestList0 `shouldBe` guestList1