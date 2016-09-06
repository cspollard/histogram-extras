{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Histogram.Extra ( histData, overflows, bins
                            , integral, scaleBy, hadd
                            , fill, Histogram, Bin(..)
                            ) where

import Control.Lens

import Data.List (mapAccumL)

import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Histogram (Histogram)
import Data.Histogram.Bin (Bin(..), BinEq(..))
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
