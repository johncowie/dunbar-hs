module Utils.Read (
  readEither
) where

import qualified Text.Read as TR
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))

typeNameFromProxy :: Typeable a => proxy a -> Either String a -> String
typeNameFromProxy proxy _ = show (typeRep proxy)

attachType :: Typeable a => Either String a -> Either String a
attachType x@(Left msg) = Left (typeNameFromProxy Proxy x ++ ": " ++ msg)
attachType x            = x

readEither :: (Read a, Typeable a) => String -> Either String a
readEither = attachType . TR.readEither
