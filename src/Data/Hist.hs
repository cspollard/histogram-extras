{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Hist
    ( Histogram, histData, bins, outOfRange
    , total, histVals, atVal, atIdx
    , histFill
    , Hist1D, Hist1DFill, hist1DFill
    , Hist2D, Hist2DFill, hist2DFill
    , Prof1D, Prof1DFill, prof1DFill
    , hadd, hadd', hdiff, hdiff', hzip, hzip'
    , removeSubHist, removeSubHist'
    , printHistogram, printDist1D, printDist2D, printBin1D, printBin2D
    , module X
    ) where

import           Control.DeepSeq
import qualified Control.Foldl               as F
import           Control.Lens
import           Data.Histogram.Cereal       ()
import           Data.Histogram.Generic      (Histogram)
import qualified Data.Histogram.Generic      as G
import           Data.Semigroup
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Fixed           as VF
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU

import           Data.Dist                   as X
import           Data.Fillable               as X
import           Data.Histogram.Bin          as X
import           Data.Histogram.Bin.Fixed    as X
import           Data.Weighted               as X



-- TODO
-- the following two methods can be removed after histogram-fill updates to have
-- strict overflows.

-- make sure we are strict in overflows.
seqMT :: Maybe (t, t1) -> Maybe (t, t1)
seqMT (Just (x, y)) = x `seq` y `seq` Just (x, y)
seqMT Nothing       = Nothing

histogramUO
  :: (Bin bin, VG.Vector v a)
  => bin -> Maybe (a, a) -> v a -> Histogram v bin a
histogramUO b = G.histogramUO b . seqMT

histData
  :: (Bin b, VG.Vector v' a)
  => Lens (Histogram v b a) (Histogram v' b a) (v a) (v' a)
histData f h = histogramUO (G.bins h) (G.outOfRange h) <$> f (G.histData h)


-- traverse over all bins and over/under flows.
histVals
  :: (VG.Vector v a, VG.Vector v c, Traversable v, Bin b)
  => Traversal (Histogram v b a) (Histogram v b c) a c
histVals f h = histogramUO b <$> uo <*> v
    where
      b = G.bins h
      v = traverse f $ G.histData h
      uo = (traverse.both) f $ G.outOfRange h


outOfRange :: (VG.Vector v a, Bin b) => Lens' (Histogram v b a) (Maybe (a, a))
outOfRange f h =
  let uo = f $ G.outOfRange h
      g uo' = histogramUO (G.bins h) uo' (G.histData h)
  in g <$> uo


bins :: (VG.Vector v a, Bin b') => Lens (Histogram v b a) (Histogram v b' a) b b'
bins f h =
  let b = f $ G.bins h
      g b' = histogramUO b' (G.outOfRange h) (G.histData h)
  in g <$> b


total :: (VG.Vector v a, Traversable v, Bin b, Monoid a) => Histogram v b a -> a
total = foldOf histVals

atVal
  :: (VG.Vector v a, Bin b)
  => BinValue b -> Traversal' (Histogram v b a) a
atVal x f h =
  let b = view bins h
      nb = nBins b
      i = toIndex b x
      k | i < 0 = (outOfRange._Just._1) f h
        | i >= nb = (outOfRange._Just._2) f h
        | otherwise = (histData.atIdx' i) f h
  in k


forceIndex :: VG.Vector v t => Int -> v t -> v t
forceIndex i v = (v VG.! i) `seq` v

atIdx :: VG.Vector v t => Int -> Traversal' (v t) t
atIdx i f v
  | i < 0 || i >= VG.length v = pure v
  | otherwise = atIdx' i f v

-- atIdx with the only bounds checking taking place in VGM.write
atIdx' :: VG.Vector v t => Int -> Traversal' (v t) t
atIdx' i f v =
    let g y = forceIndex i . VG.modify (\v' -> VGM.write v' i y)
    in f (v VG.! i) <&> flip g v


-- this evaluates the hadded histogram to WHNF before returning
-- however it does NOT explicitly evaluate all values in the underlying vector
hzip
  :: (VG.Vector v a, VG.Vector v c, VG.Vector v d, BinEq b)
  => (a -> c -> d)
  -> Histogram v b a
  -> Histogram v b c
  -> Maybe (Histogram v b d)
hzip f h h' = do
  let uo = view outOfRange h
      uo' = view outOfRange h'
      b = view bins h
  oor <- addMaybe uo uo'

  if not $ b `binEq` view bins h'
    then Nothing
    else
      let v = view histData h
          v' = view histData h'
          h'' = histogramUO b oor $ VG.zipWith f v v'
      in Just h''

  where
    addMaybe (Just (x, y)) (Just (x', y')) = Just $ Just (f x x', f y y')
    addMaybe Nothing Nothing               = Just Nothing
    addMaybe _ _                           = Nothing


-- explicitly evaluates all values in the underlying vector
hzip'
  :: ( VG.Vector v a, VG.Vector v c, VG.Vector v d, BinEq b
     , NFData b, NFData d, NFData (v d) )
  => (a -> c -> d)
  -> Histogram v b a
  -> Histogram v b c
  -> Maybe (Histogram v b d)
hzip' f h h' =
  case hzip f h h' of
    Nothing -> Nothing
    Just x  -> force $ Just x


hadd
  :: (VG.Vector v a, Semigroup a, BinEq b)
  => Histogram v b a -> Histogram v b a -> Maybe (Histogram v b a)
hadd = hzip (<>)

hadd'
  :: ( VG.Vector v a, Semigroup a, BinEq b
     , NFData b, NFData a, NFData (v a) )
  => Histogram v b a -> Histogram v b a -> Maybe (Histogram v b a)
hadd' = hzip' (<>)

hdiff
  :: (Num d, BinEq b, VG.Vector v d)
  => Histogram v b d -> Histogram v b d -> Maybe (Histogram v b d)
hdiff = hzip (-)

hdiff'
  :: ( Num d, BinEq b, VG.Vector v d
     , NFData b, NFData d, NFData (v d) )
  => Histogram v b d -> Histogram v b d -> Maybe (Histogram v b d)
hdiff' = hzip' (-)


removeSubHist
  :: ( VF.Vector v1 (v1 a), VF.Vector v1 a, Num a, BinEq b
     , VG.Vector v (DistND v1 a) )
  => Histogram v b (DistND v1 a)
  -> Histogram v b (DistND v1 a)
  -> Maybe (Histogram v b (DistND v1 a))
removeSubHist = hzip removeSubDist


removeSubHist'
  :: ( VF.Vector v1 (v1 a), VF.Vector v1 a, Num a, NFData (v (DistND v1 a))
     , NFData (v1 (v1 a)), NFData (v1 a), NFData a, NFData b, BinEq b
     , VG.Vector v (DistND v1 a) )
  => Histogram v b (DistND v1 a)
  -> Histogram v b (DistND v1 a)
  -> Maybe (Histogram v b (DistND v1 a))
removeSubHist' = hzip' removeSubDist


histFill
  :: (VG.Vector v c, Bin b)
  => Histogram v b c
  -> (a -> (d, BinValue b))
  -> (c -> d -> c)
  -> F.Fold a (Histogram v b c)
histFill h f comb = F.Fold g h id
  where
    g h'' xs =
      let (val, x) = f xs
      in over (atVal x) (`comb` val) h''


type Hist1DFill b a = F.Fold a (Hist1D b)
type Hist1D b = Histogram V.Vector b (Dist1D Double)

toVectorV :: VU.Unbox a => VU.Vector a -> V.Vector a
toVectorV = VG.convert

toVectorU :: VU.Unbox a => V.Vector a -> VU.Vector a
toVectorU = VG.convert

hist1DFill :: (Bin b, BinValue b ~ Double) => Hist1D b -> Hist1DFill b (Double, Double)
hist1DFill h =
  let h' = over histData toVectorU h
      f = histFill h' (\(x, w) -> ((Only x, w), x)) (flip $ uncurry filling)
  in over histData toVectorV <$> f

type Hist2DFill b b' a = F.Fold a (Hist2D b b')
type Hist2D b b' = Histogram V.Vector (Bin2D b b') (Dist2D Double)

hist2DFill
  :: (Bin b, Bin b', BinValue b ~ Double, BinValue b' ~ Double)
  => Hist2D b b' -> Hist2DFill b b' ((Double, Double), Double)
hist2DFill h = histFill h (\((x, y), w) -> ((Pair x y, w), (x, y))) (flip $ uncurry filling)

type Prof1DFill b a = F.Fold a (Prof1D b)
type Prof1D b = Histogram V.Vector b (Dist2D Double)

prof1DFill :: (Bin b, BinValue b ~ Double) => Prof1D b -> Prof1DFill b ((Double, Double), Double)
prof1DFill h = histFill h (\((x, y), w) -> ((Pair x y, w), x)) (flip $ uncurry filling)



printDist1D :: Show a => Dist1D a -> Text
printDist1D d =
  T.intercalate "\t" . fmap T.pack
    $ [ views sumW show d
      , views sumWW show d
      , views sumWX (show.fromOnly) d
      , views sumWXY (show.fromOnly.fromOnly) d
      , views nentries show d
      ]
  where fromOnly (Only x) = x

printDist2D :: Show a => Dist2D a -> Text
printDist2D d =
  T.intercalate "\t" . map T.pack
    $ [ views sumW show d
      , views sumWW show d
      , views (sumWX._1) show d
      , views (sumWXY._1._1) show d
      , views (sumWX._2) show d
      , views (sumWXY._2._2) show d
      , views nentries show d
      ]

printBin1D :: Show a => (a, a) -> Text
printBin1D (x, y) = showT x <> "\t" <> showT y

printBin2D :: Show a => ((a, a), (a, a)) -> Text
printBin2D ((xl, yl), (xh, yh)) =
  T.intercalate "\t" . fmap showT
    $ [xl, xh, yl, yh]

showT :: Show a => a -> Text
showT = T.pack . show


printHistogram
  :: ( IntervalBin b, VG.Vector v Text, VG.Vector v (BinValue b, BinValue b)
     , VG.Vector v t, Monoid t, Traversable v, Show (BinValue b) )
  => (t -> Text) -> ((BinValue b, BinValue b) -> Text) -> Histogram v b t -> Text
printHistogram showContents showInterval h =
  T.unlines
    $ "Total\tTotal\t" <> showContents (total h)
      : maybe ls (\x -> uo x ++ ls) (view outOfRange h)

  where
    uo (u, o) =
      [ "Underflow\tUnderflow\t" <> showContents u
      , "Overflow\tOverflow\t" <> showContents o
      ]

    ls = VG.toList (VG.zipWith f bs (view histData h))

    f bi d = showInterval bi <> "\t" <> showContents d

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


instance (IntervalBin x, IntervalBin y) => IntervalBin (Bin2D x y) where
  binInterval b@(Bin2D bx by) j =
    let (jx, jy) = toIndex2D b j
        (xmn, xmx) = binInterval bx jx
        (ymn, ymx) = binInterval by jy
    in ((xmn, ymn), (xmx, ymx))
