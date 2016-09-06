{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.YODA.Dist ( Dist0D, sumW, sumWW, nentries
                      , Dist1D, sumX, sumWX, distW
                      , Dist2D, distX, distY, sumWXY
                      , printDist0D, printDist1D
                      ) where

import Control.Lens
import GHC.Generics

import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T

import Data.Serialize

import Data.Weighted
import Data.Fillable


data Dist0D a = Dist0D { _sumW :: !a
                       , _sumWW :: !a
                       , _nentries :: Int
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
    w `fill` d = d & sumW +~ w
                   & sumWW +~ (w*w)


printDist0D :: Show a => Dist0D a -> Text
printDist0D (Dist0D w ww n) = T.concat [ T.pack (show w), "\t"
                                       , T.pack (show ww), "\t"
                                       , T.pack (show n)
                                       ]


data Dist1D a = Dist1D { _distW :: !(Dist0D a)
                       , _sumX :: !a
                       , _sumWX :: !a
                       } deriving (Generic, Show)

makeLenses ''Dist1D

instance Wrapped (Dist1D a) where
    type Unwrapped (Dist1D a) = (Dist0D a, a, a)
    _Wrapped' = iso (tuple3 <$> _distW <*> _sumX <*> _sumWX) (Dist1D <$> view _1 <*> view _2 <*> view _3)

instance Num a => Semigroup (Dist1D a) where
    Dist1D dw sx swx <> Dist1D dw' sx' swx' = Dist1D (dw<>dw') (sx+sx') (swx+swx')

instance Num a => Monoid (Dist1D a) where
    mempty = Dist1D mempty 0 0
    mappend = (<>)


instance (Num a, Fractional a) => Weighted (Dist1D a) where
    type Weight (Dist1D a) = a
    d `scaledBy` w = d & distW %~ (`scaledBy` w)
                       & sumWX *~ w

    integral = lens (view $ distW . integral) (\d w -> let w' = d ^. integral in d & (distW . integral) .~ w & sumWX *~ (w'/w))


instance Num a => Fillable (Dist1D a) where
    type FillVec (Dist1D a) = (a, a)
    (w, x) `fill` d = d & distW %~ fill w
                        & sumX +~ x
                        & sumWX +~ (w*x)


printDist1D :: Show a => Dist1D a -> Text
printDist1D (Dist1D dw sx swx) = T.concat [ T.pack (views sumW show dw), "\t"
                                          , T.pack (views sumWW show dw), "\t"
                                          , T.pack (show sx), "\t"
                                          , T.pack (show swx)
                                          , T.pack (views nentries show dw), "\t"
                                          ]


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

    integral = lens (view $ distX . integral) (\d w -> let w' = d ^. integral
                                                       in  d & (distX . integral) .~ w
                                                             & (distY . integral) .~ w
                                                             & sumWXY *~ (w'/w))


instance Num a => Fillable (Dist2D a) where
    type FillVec (Dist2D a) = (a, a, a)
    (w, x, y) `fill` d = d & distX %~ fill (w, x)
                           & distY %~ fill (w, y)
                           & sumWXY +~ (w*x*y)



instance Serialize a => Serialize (Dist0D a) where
instance Serialize a => Serialize (Dist1D a) where
instance Serialize a => Serialize (Dist2D a) where
