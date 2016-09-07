{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Hist.Internal where

import Control.Lens

import Data.Histogram.Generic (Histogram)
import qualified Data.Histogram.Generic as H

import Data.Histogram.Bin (Bin(..), BinEq(..))

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

import Data.Semigroup (Semigroup(..))

import Data.Fillable (Fillable, FillVec)
import qualified Data.Fillable as F


-- strict version of modify
modify' :: Vector v a => (a -> a) -> Int -> v a -> v a
modify' f i = V.modify $ \v -> do y <- MV.read v i
                                  MV.write v i $! f y
                                  return ()


histData :: (Vector v a, Bin b) => Lens' (Histogram v b a) (v a)
histData = lens H.histData f
    where f h = H.histogramUO (view bins h) (view overflows h)


overflows :: (Vector v a, Bin b) => Lens' (Histogram v b a) (Maybe (a, a))
overflows = lens (\g -> (,) <$> H.underflows g <*> H.overflows g) f
    where f h uo = H.histogramUO (view bins h) uo (view histData h)


bins :: (Vector v a, Bin b) => Lens' (Histogram v b a) b
bins = lens H.bins f
    where f h bs = H.histogramUO bs (view overflows h) (view histData h)


hadd :: (Vector v a, Semigroup a, Bin b, BinEq b) => Histogram v b a -> Histogram v b a -> Histogram v b a
hadd h h' | H.bins h `binEq` H.bins h' = over histData (V.zipWith (<>) (view histData h')) h
hadd _ _                               = error "attempt to add histograms with different binning."


fill :: (Vector v a, Bin b, Fillable a) => FillVec a -> BinValue b -> Histogram v b a -> Histogram v b a
fill wxs x h = case ixH of
                    Just i' -> over histData (modify' (F.fill wxs) i') h
                    Nothing -> h
    where b = view bins h
          ou = view overflows h
          n = H.nBins b
          i = H.toIndex b x
          ixH | i < 0     = const n <$> ou     -- underflow
              | i >= n    = const (n+1) <$> ou -- overflow
              | otherwise = Just i
