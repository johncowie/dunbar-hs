module Data.FriendSpec (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Data.Friend

main :: IO ()
main = hspec $ do
  describe "Friend spec" $ do
    it "Should correctly pretty print friend" $ do
      let friend = newFriend "Thom" "Yorke" ["Singer in Radiohead", "Looks like a beaten-up elf"]
      shouldBe (prettyPrint "ID1" friend)
               "---------------------------------\n\
               \| ID: ID1                       |\n\
               \---------------------------------\n\
               \| Name: Thom Yorke              |\n\
               \---------------------------------\n\
               \| Notes:                        |\n\
               \|  - Singer in Radiohead        |\n\
               \|  - Looks like a beaten-up elf |\n\
               \---------------------------------"
