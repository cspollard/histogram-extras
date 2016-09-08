{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Prof ( Prof1D, prof, profData, bins, overflows
                 , prof1D, printProf1D
                 , addP
                 , Bin(..), BinD, binD
                 , module X
                 ) where

import Control.Lens
import Data.Foldable (fold)
import Data.Semigroup ((<>))

import Data.Serialize
import GHC.Generics
import Data.Histogram.Cereal ()

import Data.Text (Text)
import qualified Data.Text as T

import Data.Histogram.Generic (Histogram, histogram)
import Data.Histogram.Bin (binsList, BinEq(..), BinD, Bin(..), IntervalBin(..), binD)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Fillable as X
import Data.Dist as X
import qualified Data.Hist.Internal as I


newtype Prof1D b a = Prof1D { _prof :: Histogram Vector b (Dist2D a) } deriving Generic

prof1D :: (Bin b, Num a) => b -> Prof1D b a
prof1D b = Prof1D $ histogram b (V.replicate (nBins b) mempty)

makeLenses ''Prof1D

instance (Show a, Show (BinValue b), Show b, Bin b) => Show (Prof1D b a) where
    show = views prof show

profData :: (Bin b) => Lens' (Prof1D b a) (Vector (Dist2D a))
profData = prof . I.histData

bins :: (Bin b) => Lens' (Prof1D b a) b
bins = prof . I.bins

overflows :: (Bin b) => Lens' (Prof1D b a) (Maybe (Dist2D a, Dist2D a))
overflows = prof . I.overflows

instance (Num a, Bin b, BinValue b ~ a) => Fillable (Prof1D b a) where
    type FillVec (Prof1D b a) = FillVec (Dist2D a)
    fill (w, (x, y)) = over prof (I.fill (w, (x, y)) x)


instance (Bin b, Serialize b, Serialize a) => Serialize (Prof1D b a) where


addP :: (Num a, Bin b, BinEq b) => Prof1D b a -> Prof1D b a -> Prof1D b a
addP p p' = over prof (views prof I.hadd p') p

printProf1D :: (Show a, Num a, Bin b, IntervalBin b, Show (BinValue b))
            => Prof1D b a -> Text
printProf1D h = T.unlines $ "Total\tTotal\t" <> showBin (fold . view profData $ h)
                          : case view overflows h of
                                 Nothing -> []
                                 Just (u, o) -> [ "Underflow\tUnderflow\t" <> showBin u
                                                , "Overflow\tOverflow\t" <> showBin o
                                                ]
                          ++ V.toList (V.zipWith f bs $ view profData h)

      where f (xmin, xmax) d = T.intercalate "\t" [ T.pack $ show xmin
                                                  , T.pack $ show xmax
                                                  , showBin d
                                                  ]

            showBin d = T.intercalate "\t" . map T.pack 
                                    $ [ views (distX . distW . sumW) show d
                                      , views (distX . distW . sumWW) show d
                                      , views (distX . sumWX) show d
                                      , views (distX . sumWXX) show d
                                      , views (distY . sumWX) show d
                                      , views (distY . sumWXX) show d
                                      , views (distX . distW . nentries) show d
                                      ]

            bs = views bins binsList h
