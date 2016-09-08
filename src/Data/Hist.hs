{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Hist ( Hist1D, hist, histData, bins, overflows
                 , hist1D, printHist1D
                 , addH
                 , Bin(..), BinD, binD
                 , module X
                 ) where

import Control.Lens
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.List (mapAccumL)

import GHC.Generics
import Data.Serialize
import Data.Histogram.Cereal ()

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Histogram.Generic (Histogram, histogram)
import Data.Histogram.Bin (binsList, BinEq(..), BinD, Bin(..), IntervalBin(..), binD)

import Data.Fillable as X
import Data.Weighted as X
import Data.Dist as X
import qualified Data.Hist.Internal as I


newtype Hist1D b a = Hist1D { _hist :: Histogram Vector b (Dist1D a) } deriving Generic

hist1D :: (Bin b, Num a) => b -> Hist1D b a
hist1D b = Hist1D $ histogram b (V.replicate (nBins b) mempty)


makeLenses ''Hist1D

instance (Show a, Show (BinValue b), Show b, Bin b) => Show (Hist1D b a) where
    show = views hist show

histData :: (Bin b) => Lens' (Hist1D b a) (Vector (Dist1D a))
histData = hist . I.histData

bins :: (Bin b) => Lens' (Hist1D b a) b
bins = hist . I.bins

overflows :: (Bin b) => Lens' (Hist1D b a) (Maybe (Dist1D a, Dist1D a))
overflows = hist . I.overflows


instance (Num a, Bin b, BinValue b ~ a) => Fillable (Hist1D b a) where
    type FillVec (Hist1D b a) = (a, a)
    fill (w, x) = over hist (I.fill (w, x) x)


instance (Num a, Fractional a, Bin b) => Weighted (Hist1D b a) where

    type Weight (Hist1D b a) = a

    h `scaledBy` w = over histData (V.map (`scaledBy` w)) h

    integral = lens (view integral . fold . view histData) f
        where f h w = let (s, xs) = mapAccumL (\s' x -> (s' <> x, x `scaledBy` (w / view integral s))) mempty . view histData $ h
                      in  set histData xs h

instance (Bin b, Serialize b, Serialize a) => Serialize (Hist1D b a) where


addH :: (Num a, Bin b, BinEq b) => Hist1D b a -> Hist1D b a -> Hist1D b a
addH h h' = over hist (views hist I.hadd h') h


printHist1D :: (Show a, Num a, Bin b, IntervalBin b, Show (BinValue b))
            => Hist1D b a -> Text
printHist1D h = T.unlines $ "Total\tTotal\t" <> showBin (fold . view histData $ h)
                          : case view overflows h of
                                 Nothing -> []
                                 Just (u, o) -> [ "Underflow\tUnderflow\t" <> showBin u
                                                , "Overflow\tOverflow\t" <> showBin o
                                                ]
                          ++ V.toList (V.zipWith f bs $ view histData h)

      where f (xmin, xmax) d = T.intercalate "\t" [ T.pack $ show xmin
                                                  , T.pack $ show xmax
                                                  , showBin d
                                                  ]

            showBin d = T.intercalate "\t" . map T.pack $ [ views (distW . sumW) show d
                                                          , views (distW . sumWW) show d
                                                          , views sumWX show d
                                                          , views sumWXX show d
                                                          , views (distW . nentries) show d
                                                          ]

            bs = views bins binsList h
