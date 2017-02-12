{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Histogram.Fixed
  ( module X
  ) where


import           Data.Histogram.Bin.Fixed as X
import           Data.Histogram.Generic   as X
import           Data.Proxy
import           Linear.V

instance Sized (V n a) where
  type Size (V n a) = n

instance (Size v ~ Size b) => Sized (Histogram v b a) where
  type Size (Histogram v b a) = Size v

instance (Size v ~ Size b, Dim b) => Dim (Histogram v b a) where
  reflectDim _ = reflectDim (Proxy :: Proxy b)
