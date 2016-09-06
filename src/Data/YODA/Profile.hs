{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.YODA.Profile ( YodaProfile1D, fillProfile1D, printProfile1D ) where

import Control.Lens

import GHC.Generics

import Data.Text (Text)

import qualified Data.Histogram.Generic as H
import Data.Histogram.Generic (Histogram)
import Data.Histogram.Bin (binsList, BinEq(..), BinD, Bin(..))

import Data.Vector (Vector)
import Data.Semigroup (Semigroup(..))

import Data.Fillable as X
import Data.YODA.Dist as X
import Data.YODA.Annotated as X
import qualified Data.YODA.Internal as I


newtype Profile1D b a = Profile1D { _prof :: Histogram Vector b (Dist2D a) } deriving Generic

makeLenses ''Profile1D

profData :: Bin b => Lens' (Profile1D b a) (Vector (Dist2D a))
profData = prof . I.histData

bins :: Bin b => Lens' (Profile1D b a) b
bins = prof . I.bins

overflows :: Bin b => Lens' (Profile1D b a) (Maybe (Dist2D a, Dist2D a))
overflows = prof . I.overflows

instance (Num a, Bin b, BinValue b ~ a) => Fillable (Profile1D b a) where
    type FillVec (Profile1D b a) = (a, a, a)
    (w, x, y) `fill` p = over prof (I.fill (w, x, y) x) p


addP :: (Num a, Bin b, BinEq b) => Profile1D b a -> Profile1D b a -> Profile1D b a
addP p p' = over prof (I.hadd $ view prof p') p

type YodaProfile1D = Annotated (Profile1D BinD Double)

fillProfile1D :: (Double, Double, Double) -> YodaProfile1D -> YodaProfile1D
fillProfile1D wxy = over thing (fill wxy)

printProfile1D :: YodaProfile1D -> Text
printProfile1D _ = ""
