{-# LANGUAGE TypeFamilies #-}

module Data.Fillable where

import           Data.Weighted

class Weighted a => Fillable a where
  type FillVec a :: *
  filling :: FillVec a -> Weight a -> a -> a


instance Fillable Int where
  type FillVec Int = ()
  filling _ x = (+ x)

instance Fillable Float where
  type FillVec Float = ()
  filling _ x = (+ x)

instance Fillable Double where
  type FillVec Double = ()
  filling _ x = (+ x)
