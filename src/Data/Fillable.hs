{-# LANGUAGE TypeFamilies #-}

module Data.Fillable where

import Data.Weighted

class Weighted a => Fillable a where
    type FillVec a :: *
    filling :: Weight a -> FillVec a -> a -> a


instance Fillable Int where
    type FillVec Int = Int
    filling x _ = (+ x)

instance Fillable Float where
    type FillVec Float = Float
    filling x _ = (+ x)

instance Fillable Double where
    type FillVec Double = Double
    filling x _ = (+ x)
