{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Friend (
  Friend
, newFriend
, showName
) where

import Data.Typeable (Typeable)

data Friend = Friend { firstname :: String
                     , lastname :: String
                     , notes :: [String] }
                     deriving (Show, Read, Typeable)

newFriend :: String -> String -> [String] -> Friend
newFriend = Friend

showName :: Friend -> String
showName Friend{..} = firstname ++ " " ++ lastname ++ notesStr notes
  where notesStr [] = ""
        notesStr [x] = " - " ++ x
