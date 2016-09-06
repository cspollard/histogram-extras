{-# LANGUAGE TypeFamilies #-}

module Data.Weighted where

import Control.Lens

class Weighted a where
    type Weight a :: *
    scaledBy :: Weighted a => a -> Weight a -> a
    integral :: Lens' a (Weight a)

instance Weighted Int where
    type Weight Int = Int
    scaledBy = (*)
    integral = lens id const

instance Weighted Float where
    type Weight Float = Float
    scaledBy = (*)
    integral = lens id const

instance Weighted Double where
    type Weight Double = Double
    scaledBy = (*)
    integral = lens id const
