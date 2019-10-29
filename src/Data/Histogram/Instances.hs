{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Histogram.Instances where

import           Both
import           Data.Biapplicative
import           Data.Bifunctor.Join
import           Data.Serialize
import           Linear.Vector
import Data.Functor.Compose
import Data.Functor.Identity



instance (Serialize a, Serialize b) => Serialize (Both a b)

instance (Serialize a, Serialize b) => Serialize (Either' a b)

instance (Serialize (f a), Serialize (g (f a))) => Serialize (Compose g f a)

instance (Serialize (p a a)) => Serialize (Join p a)

instance (Serialize a) => Serialize (Identity a)

instance Biapplicative b => Additive (Join b) where
  zero = pure 0
