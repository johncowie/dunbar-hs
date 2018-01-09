module Utils.ToMove (
  addErrorPrefix
) where

addErrorPrefix :: String -> Either String a -> Either String a
addErrorPrefix pfx (Left err) = Left $ pfx ++ ": " ++ err
addErrorPrefix _ x = x
