{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Dist ( Dist0D, sumW, sumWW, nentries
                 , Dist1D, sumWX, sumWXX, distW
                 , Dist2D, distX, distY, sumWXY
                 ) where

import Control.Lens
import Data.Semigroup

import GHC.Generics
import Data.Serialize

import Data.Weighted
import Data.Fillable

import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed


data Dist0D a = Dist0D { _sumW :: !a
                       , _sumWW :: !a
                       , _nentries :: !Int
                       } deriving (Generic, Show)

makeLenses ''Dist0D


-- helper function
tuple3 :: a -> b -> c -> (a, b, c)
tuple3 x y z = (x, y, z)

instance Wrapped (Dist0D a) where
    type Unwrapped (Dist0D a) = (a, a, Int)
    _Wrapped' = iso (tuple3 <$> _sumW <*> _sumWW <*> _nentries) (Dist0D <$> view _1 <*> view _2 <*> view _3)


instance Num a => Semigroup (Dist0D a) where
    Dist0D w ww n <> Dist0D w' ww' n' = Dist0D (w+w') (ww+ww') (n+n')

instance Num a => Monoid (Dist0D a) where
    mempty = Dist0D 0 0 0
    mappend = (<>)


instance (Num a, Fractional a) => Weighted (Dist0D a) where
    type Weight (Dist0D a) = a
    d `scaledBy` w = d & sumW *~ w
                       & sumWW *~ (w*w)

    integral = lens (view sumW) (\(Dist0D sw sww n) w -> Dist0D w (sww*(w/sw)^(2::Int)) n)


instance Num a => Fillable (Dist0D a) where
    type FillVec (Dist0D a) = a
    fill w d = d & sumW +~ w
                 & sumWW +~ (w*w)
                 & nentries +~ 1


data Dist1D a = Dist1D { _distW :: !(Dist0D a)
                       , _sumWX :: !a
                       , _sumWXX :: !a
                       } deriving (Generic, Show)

makeLenses ''Dist1D

instance Wrapped (Dist1D a) where
    type Unwrapped (Dist1D a) = (Dist0D a, a, a)
    _Wrapped' = iso (tuple3 <$> _distW <*> _sumWX <*> _sumWXX) (Dist1D <$> view _1 <*> view _2 <*> view _3)

instance Num a => Semigroup (Dist1D a) where
    Dist1D dw swx swxx <> Dist1D dw' swx' swxx' = Dist1D (dw<>dw') (swx+swx') (swxx+swxx')

instance Num a => Monoid (Dist1D a) where
    mempty = Dist1D mempty 0 0
    mappend = (<>)


instance (Num a, Fractional a) => Weighted (Dist1D a) where
    type Weight (Dist1D a) = a
    d `scaledBy` w = d & distW %~ (`scaledBy` w)
                       & sumWX *~ w
                       & sumWXX *~ w

    integral = lens (view $ distW . integral) (\d w -> let w' = d ^. integral in d `scaledBy` (w/w'))


instance Num a => Fillable (Dist1D a) where
    type FillVec (Dist1D a) = (a, a)
    fill (w, x) d = d & distW %~ fill w
                      & sumWX +~ (w*x)
                      & sumWXX +~ (w*x*x)


data Dist2D a = Dist2D { _distX :: !(Dist1D a)
                       , _distY :: !(Dist1D a)
                       , _sumWXY :: !a
                       } deriving (Generic, Show)


makeLenses ''Dist2D

instance Wrapped (Dist2D a) where
    type Unwrapped (Dist2D a) = (Dist1D a, Dist1D a, a)
    _Wrapped' = iso (tuple3 <$> _distX <*> _distY <*> _sumWXY) (Dist2D <$> view _1 <*> view _2 <*> view _3)

instance Num a => Semigroup (Dist2D a) where
    Dist2D dx dy swxy <> Dist2D dx' dy' swxy' = Dist2D (dx<>dx') (dy<>dy') (swxy+swxy')

instance Num a => Monoid (Dist2D a) where
    mempty = Dist2D mempty mempty 0
    mappend = (<>)


instance (Num a, Fractional a) => Weighted (Dist2D a) where
    type Weight (Dist2D a) = a
    d `scaledBy` w = d & distX %~ (`scaledBy` w)
                       & distY %~ (`scaledBy` w)
                       & sumWXY *~ w

    integral = lens (view $ distX . integral) (\d w -> let w' = d ^. integral in d `scaledBy` (w/w'))


instance Num a => Fillable (Dist2D a) where
    type FillVec (Dist2D a) = (a, (a, a))
    fill (w, (x, y)) d = d & distX %~ fill (w, x)
                           & distY %~ fill (w, y)
                           & sumWXY +~ (w*x*y)


instance Serialize a => Serialize (Dist0D a) where
instance Serialize a => Serialize (Dist1D a) where
instance Serialize a => Serialize (Dist2D a) where

derivingUnbox "Dist0D"
    [t| forall a. (Unbox a) => Dist0D a -> (a, a, Int) |]
    [| view _Wrapped' |]
    [| view _Unwrapped' |]

derivingUnbox "Dist1D"
    [t| forall a. (Unbox a) => Dist1D a -> (Dist0D a, a, a) |]
    [| view _Wrapped' |]
    [| view _Unwrapped' |]

derivingUnbox "Dist2D"
    [t| forall a. (Unbox a) => Dist2D a -> (Dist1D a, Dist1D a, a) |]
    [| view _Wrapped' |]
    [| view _Unwrapped' |]
