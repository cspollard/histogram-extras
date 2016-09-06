{-# LANGUAGE TypeFamilies #-}

module Data.Fillable where

class Fillable a where
    type FillVec a :: *
    fill :: FillVec a -> a -> a


instance Fillable Int where
    type FillVec Int = Int
    fill = (+)

instance Fillable Float where
    type FillVec Float = Float
    fill = (+)

instance Fillable Double where
    type FillVec Double = Double
    fill = (+)
