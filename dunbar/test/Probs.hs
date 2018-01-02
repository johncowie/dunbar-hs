{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}

module Probs () where

class (Monad m) => Wrapper m a where
  valM :: m (Maybe a)
  val  :: m a

f :: (Wrapper m a) => (forall a. m a -> IO a) -> IO (a, Maybe a)
f g = do
  x <- g val
  yM <- g valM
  return (x, yM)

-- example usage

instance Wrapper [] Int where
  val = [2]
  valM = [Just 3]

g :: [a] -> IO a
g = return . head
