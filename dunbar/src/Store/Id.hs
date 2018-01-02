module Store.Id (
  nextId
) where

import Data.List (sort)
import Text.Read (readEither)

nextId :: [(String, a)] -> Either String String
nextId [] = Right (show 0)
nextId rs = do
  ints <- sequence . map readEither . map fst $ rs
  return . show . (+ 1) . last . sort $ ints
