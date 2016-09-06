{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.YODA.Histo ( Histo1D, hist, histData, bins, overflows
                       , YodaHisto1D, yodaHisto1D, fillHisto1D, printHisto1D
                       , addH, addYH
                       , Bin(..), BinD, binD
                       , module X
                       ) where

import Control.Lens

import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T

import Data.Histogram.Generic (Histogram, histogram)
import Data.Histogram.Bin (binsList, BinEq(..), BinD, Bin(..), binD)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Foldable (fold)
import Data.List (mapAccumL)
import Data.Semigroup ((<>))

import Data.Serialize
import Data.Histogram.Cereal ()

import Data.Fillable as X
import Data.Weighted as X
import Data.YODA.Dist as X
import Data.YODA.Annotated as X
import qualified Data.YODA.Internal as I


newtype Histo1D b a = Histo1D { _hist :: Histogram Vector b (Dist1D a) } deriving Generic

makeLenses ''Histo1D

histData :: Bin b => Lens' (Histo1D b a) (Vector (Dist1D a))
histData = hist . I.histData

bins :: Bin b => Lens' (Histo1D b a) b
bins = hist . I.bins

overflows :: Bin b => Lens' (Histo1D b a) (Maybe (Dist1D a, Dist1D a))
overflows = hist . I.overflows


instance (Num a, Bin b, BinValue b ~ a) => Fillable (Histo1D b a) where
    type FillVec (Histo1D b a) = (a, a)
    (w, x) `fill` p = over hist (I.fill (w, x) x) p


instance (Num a, Fractional a, Bin b) => Weighted (Histo1D b a) where

    type Weight (Histo1D b a) = a

    h `scaledBy` w = over (histData . traverse) (`scaledBy` w) h

    integral = lens (view integral . fold . view histData) f
        where f h w = let (s, xs) = mapAccumL (\s' x -> (s' <> x, x `scaledBy` (w / view integral s))) mempty . view histData $ h
                      in  set histData xs h

instance (Bin b, Serialize b, Serialize a) => Serialize (Histo1D b a) where


addH :: (Num a, Bin b, BinEq b) => Histo1D b a -> Histo1D b a -> Histo1D b a
addH h h' = over hist (I.hadd $ view hist h') h


-- a YodaHisto is just a histogram with some annotations.
type YodaHisto1D = Annotated (Histo1D BinD Double)

fillHisto1D :: (Double, Double) -> YodaHisto1D -> YodaHisto1D
fillHisto1D wx = over thing (fill wx)

printHisto1D :: YodaHisto1D -> Text
printHisto1D yh = T.unlines $ let p = yh ^?! path 
                                  xl = yh ^?! xlabel
                                  yl = yh ^?! ylabel
                                  h = yh ^. thing
                                  bs = V.toList (binsList $ h ^. bins :: Vector (Double, Double))
                              in  [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                                  , "XLabel=" <> xl, "YLabel=" <> yl
                                  , "Total\tTotal\t" <> printDist1D (fold . view histData $ h)
                                  ] ++ case view overflows h of
                                            Nothing -> []
                                            Just (u, o) -> [ "Underflow\tUnderflow\t" <> printDist1D u
                                                           , "Overflow\tOverflow\t" <> printDist1D o
                                                           ]
                                    ++ zipWith f bs (V.toList $ view histData h) ++
                                  [ "# END YODA_HISTO1D", "" ]

      where f (xmin, xmax) d = T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> printDist1D d


yodaHisto1D :: Int -> Double -> Double -> YodaHisto1D
yodaHisto1D n mn mx = annotated . Histo1D $ histogram (binD mn n mx) (V.replicate n mempty)


addYH :: YodaHisto1D -> YodaHisto1D -> YodaHisto1D
addYH yh yh' = yh & thing %~ addH (view thing yh')
