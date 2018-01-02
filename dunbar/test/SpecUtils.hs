{-# LANGUAGE RankNTypes #-}

module SpecUtils (
  (==>)
) where

import Test.Hspec (Expectation, shouldBe)

(==>) :: forall a. (Eq a, Show a) => a -> a -> Expectation
(==>) = shouldBe
