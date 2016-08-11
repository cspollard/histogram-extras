{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Histogram.Extra where

import Control.Lens
import Data.Monoid ((<>))

import Data.Serialize.Text ()
import Data.Histogram.Cereal ()

import Data.List (mapAccumL)

import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Text (Text)
import qualified Data.Text as T

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.Histogram (Histogram)
import Data.Histogram.Bin (Bin, BinValue, binsList, BinEq(..))
import qualified Data.Histogram as H


mapPoints :: (Bin b, Unbox v) => (v -> v) -> Histogram b v -> Histogram b v
mapPoints = over (histData . V.mapM)


histData :: (Unbox v, Bin b) => Lens' (Histogram b v) (Vector v)
histData = lens H.histData f
    where f h = H.histogramUO (view bins h) (view overflows h)


overflows :: (Unbox v, Bin b) => Lens' (Histogram b v) (Maybe (v, v))
overflows = lens (\g -> (,) <$> H.underflows g <*> H.overflows g) f
    where f h uo = H.histogramUO (view bins h) uo (view histData h)


bins :: (Unbox v, Bin b) => Lens' (Histogram b v) b
bins = lens H.bins f
    where f h bs = H.histogramUO bs (view overflows h) (view histData h)


integral :: (Unbox v, Num v, Fractional v, Bin b) => Lens' (Histogram b v) v
integral = lens (V.sum . H.histData) f
    where f h w = let (s, xs) = mapAccumL (\s' x -> (x+s', x*w/s)) 0 . V.toList . view histData $ h
                  in  set histData (V.fromList xs) h


scaleBy :: (Num v, Unbox v, Bin b) => Histogram b v -> v -> Histogram b v
h `scaleBy` x = mapPoints (*x) h


hadd :: (Unbox v, Num v, Bin b, BinEq b) => Histogram b v -> Histogram b v -> Histogram b v
hadd h h' | H.bins h `binEq` H.bins h' = over histData (V.zipWith (+) (view histData h')) h
hadd _ _                               = error "attempt to add histograms with different binning."


modify' :: Unbox b => (b -> b) -> Int -> Vector b -> Vector b
modify' f i = V.modify $ \v -> do y <- MV.read v i
                                  MV.write v i $! f y
                                  return ()


fill :: (Unbox v, Bin b) => (a -> v -> v) -> (a, BinValue b) -> Histogram b v -> Histogram b v
fill f (w, x) h = over histData (maybe id (modify' (f w)) ixH) h 
    where b = view bins h
          ou = view overflows h
          n = H.nBins b
          i = H.toIndex b x
          ixH | i < 0     = const n <$> ou     -- underflow
              | i >= n    = const (n+1) <$> ou -- overflow
              | otherwise = Just i


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
