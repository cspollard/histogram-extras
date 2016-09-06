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

-- a YodaHisto is just a histogram with some annotations.
data YodaHisto b val = YodaHisto { _path :: Text
                                 , _xLabel :: Text
                                 , _yLabel :: Text
                                 , _yhHisto :: !(Histogram b val)
                                 } deriving Generic


makeLenses ''YodaHisto

haddYH :: (Unbox v, Num v, Bin b, BinEq b)
       => YodaHisto b v -> YodaHisto b v -> YodaHisto b v
haddYH yh yh' = over yhHisto (hadd $ view yhHisto yh') yh


type YodaHisto1D = YodaHisto H.BinD Double

instance (Serialize val, Bin b, Serialize b, G.Vector V.Vector val)
         => Serialize (YodaHisto b val) where


showHisto :: YodaHisto1D -> Text
showHisto (YodaHisto p xl yl h) = T.unlines $
                            [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                            , "XLabel=" <> xl, "YLabel=" <> yl
                            , "Total\tTotal\t" <> distToText (view integral h)
                            , case view overflows h of
                                   Nothing -> ""
                                   Just (u, o) -> "Underflow\tUnderflow\t" <> T.pack (show u) <>
                                                  "Overflow\tOverflow\t" <> T.pack (show o)
                            ] ++ zipWith f bs (V.toList $ view histData h) ++
                            [ "# END YODA_HISTO1D", "" ]

                            where f (xmin, xmax) d = T.pack (show xmin ++ "\t" ++ show xmax ++ "\t") <> distToText d
                                  distToText d = T.pack (show d) <> "\t0.0\t0.0\t0.0\t0.0"
                                  bs = V.toList (binsList $ view bins h :: Vector (Double, Double))
