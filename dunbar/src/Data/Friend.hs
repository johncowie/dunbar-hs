{-# LANGUAGE DeriveDataTypeable #-}

module Data.Friend (
  Friend
, newFriend
, showName
) where

import Data.Typeable (Typeable)
import Data.Default (Default(def))

data Friend = Friend {firstname :: String
                     ,lastname :: String} deriving (Show, Read, Typeable)

instance Default Friend where
  def = newFriend def def

newFriend :: String -> String -> Friend
newFriend = Friend

showName :: Friend -> String
showName friend = firstname friend ++ " " ++ lastname friend
