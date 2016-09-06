{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.YODA.Profile ( Profile1D, prof, profData, bins, overflows
                         , YodaProfile1D, yodaProfile1D, fillProfile1D, printProfile1D
                         , addP, addYP
                         , Bin(..), BinD, binD
                         , module X
                         ) where

import Control.Lens

import GHC.Generics

import Data.Text (Text)

import Data.Histogram.Generic (Histogram, histogram)
import Data.Histogram.Bin (BinEq(..), BinD, Bin(..), binD)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Serialize
import Data.Histogram.Cereal ()

import Data.Fillable as X
import Data.YODA.Dist as X
import Data.YODA.Annotated as X
import qualified Data.YODA.Internal as I


newtype Profile1D b a = Profile1D { _prof :: Histogram Vector b (Dist2D a) } deriving Generic

makeLenses ''Profile1D

instance (Show a, Show (BinValue b), Show b, Bin b) => Show (Profile1D b a) where
    show = views prof show

profData :: Bin b => Lens' (Profile1D b a) (Vector (Dist2D a))
profData = prof . I.histData

bins :: Bin b => Lens' (Profile1D b a) b
bins = prof . I.bins

overflows :: Bin b => Lens' (Profile1D b a) (Maybe (Dist2D a, Dist2D a))
overflows = prof . I.overflows

instance (Num a, Bin b, BinValue b ~ a) => Fillable (Profile1D b a) where
    type FillVec (Profile1D b a) = (a, a, a)
    (w, x, y) `fill` p = over prof (I.fill (w, x, y) x) p


instance (Bin b, Serialize b, Serialize a) => Serialize (Profile1D b a) where


addP :: (Num a, Bin b, BinEq b) => Profile1D b a -> Profile1D b a -> Profile1D b a
addP p p' = over prof (I.hadd $ view prof p') p

type YodaProfile1D = Annotated (Profile1D BinD Double)

fillProfile1D :: (Double, Double, Double) -> YodaProfile1D -> YodaProfile1D
fillProfile1D wxy = over thing (fill wxy)

printProfile1D :: YodaProfile1D -> Text
printProfile1D _ = ""

yodaProfile1D :: Int -> Double -> Double -> YodaProfile1D
yodaProfile1D n mn mx = annotated . Profile1D $ histogram (binD mn n mx) (V.replicate n mempty)

addYP :: YodaProfile1D -> YodaProfile1D -> YodaProfile1D
addYP yp yp' = yp & thing %~ addP (view thing yp')
