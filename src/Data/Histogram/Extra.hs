{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Histogram.Extra ( histData, overflows, bins
                            , integral, hadd
                            , fill, Histogram, Bin(..)
                            ) where

import Control.Lens

import Data.List (mapAccumL)

import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Histogram.Generic (Histogram)
import qualified Data.Histogram.Generic as H
import Data.Histogram.Bin (Bin(..), BinEq(..))
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G

import Data.Semigroup (Semigroup(..))

import Data.Weighted


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
hadd h h' | H.bins h `binEq` H.bins h' = over histData (G.zipWith (+) (view histData h')) h
hadd _ _                               = error "attempt to add histograms with different binning."


modify' :: Vector v a => (a -> a) -> Int -> v a -> v a
modify' f i = G.modify $ \v -> do y <- MV.read v i
                                  MV.write v i $! f y
                                  return ()


fill :: (Vector v a, Bin b) => (w -> a -> a) -> (w, BinValue b) -> Histogram v b a -> Histogram v b a
fill f (w, x) h = over histData (maybe id (modify' (f w)) ixH) h 
    where b = view bins h
          ou = view overflows h
          n = H.nBins b
          i = H.toIndex b x
          ixH | i < 0     = const n <$> ou     -- underflow
              | i >= n    = const (n+1) <$> ou -- overflow
              | otherwise = Just i


instance (Vector v a, Bin b, Weighted a) => Weighted (Histogram v b a) where
    type Weight (Histogram v b a) = Weight a
    h `scaledBy` w = over (histData . fmap) (`scaledBy` w)
    integral :: (Vector v a, Semigroup a, Monoid a, Weighted a, Fractional (Weight a), Bin b) => Lens' (Histogram v b a) a
    integral = lens (G.foldl (<>) mempty . H.histData) f
        where f h w = let (s, xs) = mapAccumL (\s' x -> (x <> s', x*w/s)) 0 . G.toList . view histData $ h
                      in  set histData (G.fromList xs) h

