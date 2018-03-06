module TimeSpec (
      spec 
    ) where 

import Test.Hspec

import Time

spec :: Spec 
spec = do 
    describe "ordering" $ do 
        it "accepts 0 time" $ do 
            compare (mkTime 0 0 0 0) (mkTime 2 1 3 2) `shouldBe` LT 
            compare (mkTime 0 0 0 0) (mkTime 0 0 0 0) `shouldBe` EQ
        it "compares time correctly" $ do 
            compare (mkTime 4 13 60 55) (mkTime 4 13 5 63) `shouldBe` GT
            compare (mkTime 5 5 5 22) (mkTime 5 5 5 22) `shouldBe` EQ
            compare (mkTime 3 3 5 23) (mkTime 3 3 4 32) `shouldBe` GT
            compare (mkTime 3 3 3 23) (mkTime 3 3 3 24) `shouldBe` LT


