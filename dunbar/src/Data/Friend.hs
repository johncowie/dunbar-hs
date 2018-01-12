{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Friend (
  Friend
, newFriend
, showName
, addNote
, prettyPrint
) where

import Data.Typeable (Typeable)
import Data.List (intercalate)
import Debug.Trace (trace)

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

traceIt :: (Show a) => a -> a
traceIt a = trace (show a) a

idLine :: String -> Friend -> [String]
idLine i = const ["ID: " ++ i]

nameLine :: Friend -> [String]
nameLine Friend{..} = ["Name: " ++ firstname ++ " " ++ lastname]

noteLines :: Friend -> [String]
noteLines Friend{..} = "Notes: ":(map (" - " ++) notes)

printGrid :: [(a -> [String])] -> a -> String
printGrid fs a = intercalate "\n" ls
  where lineGroups = map (\f -> f a) fs
        width = 4 + (maximum $ map length $ concat lineGroups)
        border = [take width $ repeat '-']
        ls = border ++ intercalate border (map (map (wrapLine width)) lineGroups) ++ border

prettyPrint :: String -> Friend -> String
prettyPrint friendId = printGrid [idLine friendId, nameLine, noteLines]

pad :: Int -> String -> String
pad w s
  | w <= l = s
  | otherwise = s ++ take (w - l) (repeat ' ')
  where l = length s

wrapLine :: Int -> String -> String
wrapLine w s = "| " ++ pad (w-4) s ++ " |"
