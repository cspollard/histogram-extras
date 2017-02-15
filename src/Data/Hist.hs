{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Hist
    ( Histogram, histData, bins, overflows
    , total, histDataUO
    , addH, printHistogram, printDist1D, printDist2D
    , module X
    ) where

import           Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup
import           Data.Histogram.Cereal    ()
import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import           Data.Histogram.Generic   (Histogram)
import qualified Data.Histogram.Generic   as G
import qualified Data.Hist.Internal       as I

-- import           Data.Weighted as X
-- import           Data.Fillable as X
import           Data.Histogram.Bin.Fixed as X
import           Data.Histogram.Bin as X
import           Data.Dist as X


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


atVal :: (Vector v a, Bin b) => BinValue b -> Lens' (Histogram v b a) a
atVal x f h =
  let b = view bins h
      nb = nBins bins
      i = toIndex b x
  in case i `compare` nb of
    GT -> over (overflows._Just._1) f h
    LT -> over (overflows._Just._2) f h
    EQ -> over (histData.element i) f h

printDist1D :: Show a => Dist1D a -> Text
printDist1D d =
  T.intercalate "\t" . map T.pack
    $ [ views (distW . sumW) show d
      , views (distW . sumWW) show d
      , views sumWX show d
      , views sumWXX show d
      , views (distW . nentries) show d
      ]

printDist2D :: Show a => Dist2D a -> Text
printDist2D d =
  T.intercalate "\t" . map T.pack
    $ [ views (distX . distW . sumW) show d
      , views (distX . distW . sumWW) show d
      , views (distX . sumWX) show d
      , views (distX . sumWXX) show d
      , views (distY . sumWX) show d
      , views (distY . sumWXX) show d
      , views (distX . distW . nentries) show d
      ]

printHistogram
  :: ( IntervalBin b, Vector v Text, Vector v (BinValue b, BinValue b)
     , Vector v t, Monoid t, Traversable v, Show (BinValue b) )
  => (t -> Text) -> Histogram v b t -> Text
printHistogram showContents h =
  T.unlines
    $ "Total\tTotal\t" <> showContents (total h)
      : maybe ls (\x -> uo x ++ ls) (view overflows h)

  where
    uo (u, o) =
      [ "Underflow\tUnderflow\t" <> showContents u
      , "Overflow\tOverflow\t" <> showContents o
      ]

    ls = VG.toList (VG.zipWith f bs (view histData h))

    f (xmin, xmax) d =
      T.intercalate "\t"
        [ T.pack $ show xmin
        , T.pack $ show xmax
        , showContents d
        ]

    bs = views bins binsList h

-- instance (Weighted a, Fractional (Weight a), Bin b, Traversable v, Vector v a, Semigroup a, Monoid a)
--     => Weighted (Histogram v b a) where
--
--     type Weight (Histogram v b a) = Weight a
--
--     scaling = over histDataUO . scaling
--
--     integral = lens getInt normTo
--         where
--             normTo h w =
--               let (s, h') = mapAccumLOf histDataUO (\s' x -> (s' <> x, scaling i x)) mempty h
--                   i = w / view integral s
--               in h'
--
--             getInt = view integral . total
--
--
-- instance
--   ( Semigroup a, Monoid a, Weighted a, Fractional (Weight a), Fillable a
--   , Traversable v, Vector v a, Bin b, BinValue b ~ FillVec a )
--   => Fillable (Histogram v b a) where
--
--     type FillVec (Histogram v b a) = FillVec a
--     filling w x = I.filling w x x
