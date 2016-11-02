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

import Data.Weighted
import Data.Fillable hiding (filling)
import qualified Data.Fillable as F


histData :: (Vector v a, Bin b) => Lens' (Histogram v b a) (v a)
histData = lens H.histData f
    where f h = H.histogramUO (view bins h) (view overflows h)


overflows :: (Vector v a, Bin b) => Lens' (Histogram v b a) (Maybe (a, a))
overflows = lens H.outOfRange f
    -- force uo to be strict
    where
        f h uo =
            let g = H.histogramUO (view bins h) uo (view histData h)
            in case uo of
                Just (x, y) -> x `seq` y `seq` g
                Nothing -> g


bins :: (Vector v a, Bin b) => Lens' (Histogram v b a) b
bins = lens H.bins f
    where f h bs = H.histogramUO bs (view overflows h) (view histData h)


hadd :: (Vector v a, Semigroup a, Bin b, BinEq b)
     => Histogram v b a -> Histogram v b a -> Histogram v b a
hadd h h' | H.bins h `binEq` H.bins h' =
    over histData (V.zipWith (<>) (view histData h')) h
          | otherwise = error "attempt to add histograms with different binning."


filling :: (Vector v a, Bin b, Fillable a)
        => Weight a -> FillVec a -> BinValue b -> Histogram v b a -> Histogram v b a
filling w xs x h
    | i < 0     = over (overflows._Just._1) (F.filling w xs) h
    | i < n     = over histData (V.modify (\mv -> MV.write mv i . F.filling w xs =<< MV.read mv i)) h
    | otherwise = over (overflows._Just._2) (F.filling w xs) h

    where b = view bins h
          n = H.nBins b
          i = H.toIndex b x
