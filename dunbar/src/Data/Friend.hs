{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Friend (
  Friend
, newFriend
, showName
, addNote
) where

import Data.Typeable (Typeable)

data Friend = Friend { firstname :: String
                     , lastname :: String
                     , notes :: [String] }
                     deriving (Show, Read, Typeable)

newFriend :: String -> String -> [String] -> Friend
newFriend = Friend

addNote :: String -> Friend -> Friend
addNote s f = f{notes=notes f ++ [s]}

showName :: Friend -> String
showName Friend{..} = firstname ++ " " ++ lastname ++ notesStr notes
  where notesStr [] = ""
        notesStr (x:xs) = " - " ++ x
