{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.ToMove (
  readEither
, addErrorPrefix
) where

import qualified Text.Read as T
import Data.Typeable (Typeable, typeOf)
import Data.Default (Default, def)

readEither :: forall a. (Default a, Typeable a, Read a) => String -> Either String a
readEither s = addErrorPrefix ("Error parsing <" ++ s ++ "> as " ++ show t ++ ".") $ T.readEither s
  where d :: a = def
        t = typeOf d

addErrorPrefix :: String -> Either String a -> Either String a
addErrorPrefix pfx (Left err) = Left $ pfx ++ ": " ++ err
addErrorPrefix _ x = x
