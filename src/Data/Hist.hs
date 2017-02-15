{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Hist
    ( Histogram, histData, bins, outOfRange
    , total, histVals, atVal, atIdx
    , histFill
    , Hist1D, Hist1DFill, hist1DFill
    , Prof1D, Prof1DFill, prof1DFill
    , hadd, printHistogram, printDist1D, printDist2D
    , module X
    ) where

import qualified Control.Foldl            as F
import           Control.Lens
import           Data.Histogram.Cereal    ()
import           Data.Histogram.Generic   (Histogram)
import qualified Data.Histogram.Generic   as G
import           Data.Semigroup
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Data.Vector.Generic      as VG
import qualified Data.Vector.Unboxed      as VU

import           Data.Dist                as X
import           Data.Fillable            as X
import           Data.Histogram.Bin       as X
import           Data.Histogram.Bin.Fixed as X
import           Data.Weighted            as X


histData :: (Bin b, VG.Vector v' a)
          => Lens (Histogram v b a) (Histogram v' b a) (v a) (v' a)
histData f h = G.histogramUO (G.bins h) (G.outOfRange h) <$> f (G.histData h)

histVals
  :: (VG.Vector v a, Traversable v, Bin b)
  => Traversal' (Histogram v b a) a
histVals f h = G.histogramUO b <$> uo <*> v
    where
      b = G.bins h
      v = traverse f $ G.histData h
      uo = (traverse.both) f $ G.outOfRange h


outOfRange :: (VG.Vector v a, Bin b) => Lens' (Histogram v b a) (Maybe (a, a))
outOfRange f h =
  let uo = f $ G.outOfRange h
      g uo' = G.histogramUO (G.bins h) uo' (G.histData h)
  in g <$> uo


bins :: (VG.Vector v a, Bin b') => Lens (Histogram v b a) (Histogram v b' a) b b'
bins f h =
  let b = f $ G.bins h
      g b' = G.histogramUO b' (G.outOfRange h) (G.histData h)
  in g <$> b

total :: (VG.Vector v a, Traversable v, Bin b, Monoid a) => Histogram v b a -> a
total = foldOf histVals

hadd
  :: (VG.Vector v a, Semigroup a, BinEq b)
  => Histogram v b a -> Histogram v b a -> Maybe (Histogram v b a)
hadd h h' = do
  let uo = view outOfRange h
      uo' = view outOfRange h'
      b = view bins h
  oor <- addMaybe uo uo'

  if not $ b `binEq` view bins h'
    then Nothing
    else
      let v = view histData h
          v' = view histData h'
      in Just . G.histogramUO b oor $ VG.zipWith (<>) v v'

  where
    addMaybe (Just (x, y)) (Just (x', y')) = Just $ Just (x<>x', y<>y')
    addMaybe Nothing Nothing               = Just Nothing
    addMaybe _ _                           = Nothing


atVal
  :: (VG.Vector v a, Bin b)
  => BinValue b -> Traversal' (Histogram v b a) a
atVal x f h =
  let b = view bins h
      nb = nBins b
      i = toIndex b x
      k | i < 0 = (outOfRange._Just._1) f h
        | i >= nb = (outOfRange._Just._2) f h
        | otherwise = (histData . atIdx i) f h
  in k

atIdx :: (VG.Vector v t, Applicative f) => Int -> (t -> f t) -> v t -> f (v t)
atIdx i f v =
  case v VG.!? i of
    Just x  -> (\y -> v VG.// [(i, y)]) <$> f x
    Nothing -> pure v

type Hist1DFill b a = F.Fold a (Hist1D b)
type Hist1D b = Histogram V.Vector b (Dist1D Double)

type Prof1DFill b a = F.Fold a (Prof1D b)
type Prof1D b = Histogram V.Vector b (Dist2D Double)

-- convert everything to Unbox vectors in order to be sure updating is strict
histFill
  :: forall v a b c d. (VG.Vector v c, VU.Unbox c, Bin b)
  => Histogram v b c
  -> (a -> (BinValue b, d))
  -> (c -> d -> c)
  -> F.Fold a (Histogram v b c)
histFill h f comb = F.Fold g h' (over histData VG.convert)
  where
    h' :: Histogram VU.Vector b c
    h' = over histData VG.convert h
    g h'' xs =
      let (x, val) = f xs
      in over (atVal x) (`comb` val) h''

hist1DFill :: (Bin b, BinValue b ~ Double) => Hist1D b -> Hist1DFill b (Double, Double)
hist1DFill h = histFill h (\(x, w) -> (x, (x, w))) (flip $ uncurry filling)

prof1DFill :: (Bin b, BinValue b ~ Double) => Prof1D b -> Prof1DFill b ((Double, Double), Double)
prof1DFill h = histFill h (\((x, y), w) -> (x, ((x, y), w))) (flip $ uncurry filling)



printDist1D :: Show a => Dist1D a -> Text
printDist1D d =
  T.intercalate "\t" . fmap T.pack
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
  :: ( IntervalBin b, VG.Vector v Text, VG.Vector v (BinValue b, BinValue b)
     , VG.Vector v t, Monoid t, Traversable v, Show (BinValue b) )
  => (t -> Text) -> Histogram v b t -> Text
printHistogram showContents h =
  T.unlines
    $ "Total\tTotal\t" <> showContents (total h)
      : maybe ls (\x -> uo x ++ ls) (view outOfRange h)

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

instance
    ( Weighted a, Fractional (Weight a), Bin b, Traversable v, VG.Vector v a
    , Monoid a )
    => Weighted (Histogram v b a) where

    type Weight (Histogram v b a) = Weight a

    scaling = over histVals . scaling

    integral = lens getInt normTo
        where
            normTo h w =
              let (s, h') = mapAccumLOf histVals (\s' x -> (s' `mappend` x, scaling i x)) mempty h
                  i = w / view integral s
              in h'

            getInt = view integral . total


instance
  ( Monoid a, Weighted a, Fractional (Weight a), Fillable a
  , Traversable v, VG.Vector v a, Bin b )
  => Fillable (Histogram v b a) where

    type FillVec (Histogram v b a) = (BinValue b, FillVec a)
    filling (x, val) w = over (atVal x) (filling val w)
