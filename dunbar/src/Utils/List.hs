module Utils.List (
  updateAt
, maybeHead
, maybeTail
, page
) where

updateAt :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateAt p f [] = []
updateAt p f (x:xs) = if (p x) then (f x):(updateAt p f xs) else x:(updateAt p f xs)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just $ head xs

maybeTail :: [a] -> [a]
maybeTail [] = []
maybeTail xs = tail xs

page :: Int -> Int -> [a] -> [a]
page offset pageSize = take pageSize . drop offset
