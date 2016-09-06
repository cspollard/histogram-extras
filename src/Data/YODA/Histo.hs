{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.YODA.Histo ( YodaHisto(..), haddYH
                       , showHisto, YodaHisto1D
                       , path, xLabel, yLabel
                       , module X
                       ) where

import Control.Lens

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.Serialize.Text ()
import Data.Histogram.Cereal ()

import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T


import Data.Histogram.Bin (binsList, BinEq(..))
import qualified Data.Histogram as H

import Data.Histogram.Extra as X
import Data.YODA.Dist as X
import Data.YODA.Annotated as X

-- a YodaHisto is just a histogram with some annotations.
type YodaHisto b v = Annotated (Histogram b v)
type YodaHisto1D = YodaHisto H.BinD (Dist1D Double)
type YodaProfile1D = YodaHisto H.BinD (Dist2D Double)

haddY :: (Unbox v, Semigroup v, Bin b, BinEq b)
      => YodaHisto b v -> YodaHisto b v -> YodaHisto b v
haddY yh yh' = over thing (hadd $ view thing yh') yh


showHisto :: YodaHisto1D -> Text
showHisto yh = T.unlines $ let p = yh ^?! path 
                               xl = yh ^?! xlabel
                               yl = yh ^?! ylabel
                           in [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                              , "XLabel=" <> xl, "YLabel=" <> yl
                              , "Total\tTotal\t" <> views integral printDist1D h
                              ] ++ case view overflows h of
                                        Nothing -> []
                                        Just (u, o) -> [ "Underflow\tUnderflow\t" <> printDist1D u
                                                       , "Overflow\tOverflow\t" <> printDist1D o
                                                       ]
                                ++ zipWith f bs (V.toList $ view histData h) ++
                              [ "# END YODA_HISTO1D", "" ]

                            where f (xmin, xmax) d = T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> printDist1D d
                                  bs = V.toList (binsList $ view bins h :: Vector (Double, Double))
