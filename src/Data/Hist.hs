{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Hist
    ( Histogram, histData, bins, overflows
    , total, histDataUO
    , addH
    -- , module X
    ) where

import           Control.Lens
import           Data.Semigroup

import           Data.Histogram.Cereal    ()

import           Data.Vector.Generic (Vector)

import           Data.Histogram.Bin.Fixed
import           Data.Histogram.Generic   (Histogram)
import qualified Data.Histogram.Generic   as G

import qualified Data.Hist.Internal       as I


histData :: (Vector v a, Bin b) => Lens' (Histogram v b a) (v a)
histData = lens G.histData f
    where f h = G.histogramUO (view bins h) (view overflows h)

histDataUO
  :: (Vector v a, Traversable v, Bin b)
  => Traversal' (Histogram v b a) a
histDataUO f h = G.histogramUO b <$> uo <*> v
    where
      b = G.bins h
      v = traverse f $ G.histData h
      uo = (traverse.both) f $ G.outOfRange h


overflows :: (Vector v a, Bin b) => Lens' (Histogram v b a) (Maybe (a, a))
overflows = lens G.outOfRange f
    -- force uo to be strict
    where
        f h uo =
            let g = G.histogramUO (view bins h) uo (view histData h)
            in case uo of
                Just (x, y) -> x `seq` y `seq` g
                Nothing -> g


bins :: (Vector v a, Bin b) => Lens' (Histogram v b a) b
bins = lens G.bins f
    where f h bs = G.histogramUO bs (view overflows h) (view histData h)

total :: (Vector v a, Traversable v, Bin b, Monoid a) => Histogram v b a -> a
total = foldOf histDataUO

addH
  :: (Vector v a, Semigroup a, BinEq b)
  => Histogram v b a -> Histogram v b a -> Histogram v b a
addH = I.hadd


-- instance (Fractional a, Bin b)
--     => Weighted (Histogram v b a) where
--
--     type Weight (Histogram v b a) = a
--
--     scaling w h = h & histData %~ V.map (scaling w)
--                     & overflows._Just.both %~ scaling w
--
--     integral = lens getInt normTo
--         where
--             normTo h w =
--                 let (s, xs) = h &
--                         mapAccumL (\s' x -> (s' <> x, scaling i x)) mempty
--                         . V.toList
--                         . view histData
--                     (t, uo) = case view overflows h of
--                                     Just (x, y) -> (s <> x <> y, Just (scaling i x, scaling i y))
--                                     Nothing     -> (s, Nothing)
--
--                     i = w / view integral t
--
--                 in h & histData .~ V.fromList xs & overflows .~ uo
--
--             getInt = view integral . total
--
--
-- instance (Fractional a, Bin b, BinValue b ~ FillVec (Dist1D a))
--     => Fillable (Histogram v b a) where
--
--     type FillVec (Histogram v b a) = a
--     filling w x = over hist (I.filling w x x)

-- printHistogram :: (Show a, Num a, IntervalBin b, Show (BinValue b))
--             => Histogram v b a -> Text
-- printHistogram h = T.unlines $ "Total\tTotal\t" <> showBin (total h)
--                           : case view overflows h of
--                                  Nothing -> []
--                                  Just (u, o) -> [ "Underflow\tUnderflow\t" <> showBin u
--                                                 , "Overflow\tOverflow\t" <> showBin o
--                                                 ]
--                           ++ zipWith f bs (VG.toList $ view histData h)
--
--       where f (xmin, xmax) d = T.intercalate "\t" [ T.pack $ show xmin
--                                                   , T.pack $ show xmax
--                                                   , showBin d
--                                                   ]
--
--             showBin d = T.intercalate "\t" . map T.pack $ [ views (distW . sumW) show d
--                                                           , views (distW . sumWW) show d
--                                                           , views sumWX show d
--                                                           , views sumWXX show d
--                                                           , views (distW . nentries) show d
--                                                           ]
--
--             bs = VG.toList $ views bins binsList h
