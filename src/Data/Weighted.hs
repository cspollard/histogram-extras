{-# LANGUAGE TypeFamilies #-}

module Data.Weighted where

import Control.Lens

class Weighted a where
    type Weight a :: *
    scaling :: Weight a -> a -> a
    integral :: Lens' a (Weight a)

instance Weighted Int where
    type Weight Int = Int
    scaling = (*)
    integral = lens id const

instance Weighted Float where
    type Weight Float = Float
    scaling = (*)
    integral = lens id const

instance Weighted Double where
    type Weight Double = Double
    scaling = (*)
    integral = lens id const
