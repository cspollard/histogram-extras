{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.YODA.Histo ( YodaHisto
                       , YodaHisto1D, printHisto1D
                       , YodaProfile1D, printProfile1D
                       , haddY
                       , module X
                       ) where

import Control.Lens

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Semigroup (Semigroup(..))

import Data.Text (Text)
import qualified Data.Text as T

import Data.Foldable

import Data.Histogram.Bin (binsList, BinEq(..), BinD)

import Data.Histogram.Extra as X
import Data.YODA.Dist as X
import Data.YODA.Annotated as X

-- a YodaHisto is just a histogram with some annotations.
type YodaHisto b a = Annotated (Histogram Vector b a)
type YodaHisto1D = YodaHisto BinD (Dist1D Double)
type YodaProfile1D = YodaHisto BinD (Dist2D Double)

haddY :: (Semigroup a, Bin b, BinEq b)
      => YodaHisto b a -> YodaHisto b a -> YodaHisto b a
haddY yh yh' = over thing (hadd $ view thing yh') yh


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


printProfile1D :: YodaProfile1D -> Text
printProfile1D _ = ""
