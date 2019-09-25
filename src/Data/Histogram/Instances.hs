{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Data.Histogram.Instances where

import Data.Serialize
import Data.Both
import Data.Bifunctor.Join
import Data.Biapplicative
import Linear.Vector

instance (Serialize a, Serialize b) => Serialize (Both a b)

instance Biapplicative b => Additive (Join b) where
  zero = pure 0
