module SpecUtils (
  (==>)
) where

import Test.Hspec (Exceptation, shouldBe)

(==>) :: forall a. (Eq a, Show a) => a -> a -> Expectation
(==>) = shouldBe
