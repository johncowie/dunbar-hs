module Utils.ListSpec (main) where

import Utils.List
import Test.Hspec (hspec, describe, it, shouldBe, shouldReturn)
import SpecUtils ((==>))

main :: IO ()
main = hspec $ do
  describe "utility tests" $ do
    it "can page through a list correctly" $ do
      page 0 0 [] ==> ([] :: [Int])
      page 0 0 [1, 2, 3] ==> ([] :: [Int])
      page 0 2 [1, 2, 3] ==> [1, 2]
      page 1 2 [1, 2, 3] ==> [2, 3]
      page 3 3 [1..20] ==> [4, 5, 6]
      page 18 3 [1..20] ==> [19, 20]
      page 20 3 [1..20] ==> []
