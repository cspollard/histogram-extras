{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Hist
    ( Hist1D, hist, histData, bins, overflows
    , total, binContents
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

import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V

import Data.Histogram (Histogram, histogramUO)
import Data.Histogram.Bin (binsList, BinEq(..), BinD, Bin(..), IntervalBin(..), binD)

import Data.Weighted as X
import Data.Fillable as X
import Data.Dist as X
import qualified Data.Hist.Internal as I


newtype Hist1D b a = Hist1D { _hist :: Histogram b (Dist1D a) } deriving Generic

hist1D :: (Bin b, Num a, Unbox a) => b -> Hist1D b a
hist1D b = Hist1D $ histogramUO b (Just (mempty, mempty)) (V.replicate (nBins b) mempty)

makeLenses ''Hist1D

instance (Show a, Unbox a, Show (BinValue b), Show b, Bin b) => Show (Hist1D b a) where
    show = views hist show

instance (Bin b, Serialize b, Serialize a, Unbox a) => Serialize (Hist1D b a) where

histData :: (Bin b, Unbox a) => Lens' (Hist1D b a) (Vector (Dist1D a))
histData = hist . I.histData

bins :: (Bin b, Unbox a) => Lens' (Hist1D b a) b
bins = hist . I.bins

overflows :: (Bin b, Unbox a) => Lens' (Hist1D b a) (Maybe (Dist1D a, Dist1D a))
overflows = hist . I.overflows

total :: (Bin b, Unbox a, Num a) => Hist1D b a -> Dist1D a
total = fold . binContents

binContents :: (Bin b, Unbox a) => Hist1D b a -> [Dist1D a]
binContents h = u ++ hd ++ o
    where
        (u, o) = case view overflows h of
                    Just (x, y) -> ([x], [y])
                    Nothing     -> ([], [])
        hd = views histData V.toList h

instance (Fractional a, Unbox a, Bin b)
    => Weighted (Hist1D b a) where

    type Weight (Hist1D b a) = a

    scaling w = over histData (V.map $ scaling w)

    integral = lens getInt normTo
        where
            normTo h w =
                let (s, xs) = h &
                        mapAccumL (\s' x -> (s' <> x, scaling i x)) mempty
                        . V.toList
                        . view histData
                    (t, uo) = case view overflows h of
                                    Just (x, y) -> (s <> x <> y, Just (scaling i x, scaling i y))
                                    Nothing     -> (s, Nothing)

                    i = w / view integral t

                in h & histData .~ V.fromList xs & overflows .~ uo

            getInt = view integral . total


instance (Fractional a, Unbox a, Bin b, BinValue b ~ FillVec (Dist1D a))
    => Fillable (Hist1D b a) where

    type FillVec (Hist1D b a) = a
    filling w x = over hist (I.filling w x x)


addH :: (Num a, Unbox a, BinEq b) => Hist1D b a -> Hist1D b a -> Hist1D b a
addH h h' = over hist (views hist I.hadd h') h


printHist1D :: (Show a, Num a, Unbox a, IntervalBin b, Show (BinValue b), Unbox (BinValue b))
            => Hist1D b a -> Text
printHist1D h = T.unlines $ "Total\tTotal\t" <> showBin (fold . V.toList . view histData $ h)
                          : case view overflows h of
                                 Nothing -> []
                                 Just (u, o) -> [ "Underflow\tUnderflow\t" <> showBin u
                                                , "Overflow\tOverflow\t" <> showBin o
                                                ]
                          ++ zipWith f bs (V.toList $ view histData h)

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

            bs = V.toList $ views bins binsList h
