{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Hist where


import Analysis.Fold
import Control.Lens.Indexed
import Data.Both
import Data.Functor.Compose
import Data.Profunctor.Optic
import Data.Moore
import Data.List (findIndex)
import Data.IntMap.Strict as IM


type Histogram i b = Compose (Both (i -> b))


pattern Histogram :: (i -> b) -> v a -> Histogram i b v a
pattern Histogram f v = Compose (Both f v)


histogram :: FunctorWithIndex i v => (i -> b) -> v a -> Histogram i b v a
histogram f = Compose . Both f


inf, neginf :: Fractional a => a
inf = 1/0
neginf = negate inf


binning :: (Fractional x, Ord x) => [x] -> Both (x -> Int) (Int -> (x, x))
binning xs = Both go go'
  where
    go x =
      maybe (error "in Data.Hist: we should always find a bin index.") (\y -> y - 1)
      $ findIndex (> x) l1

    go' i = l2 !! i

    ranges (y:y':ys) = (y, y') : ranges (y':ys)
    ranges [_] = []
    ranges [] = []

    l1 = (neginf : xs) ++ pure inf
    l2 = ranges l1


histMoore1D
  :: (Fractional x, Ord x)
  => [x] -> Moore' (x, a) (Histogram Int (x, x) IM.IntMap Int)
histMoore1D xs =
  let Both idx range = binning xs
  in layerF
      (\(x, a) -> (a, ixH (idx x)))
      (histogram range (IM.fromList $ zip [0 .. length xs] (repeat $ counter 0)))


bins :: FunctorWithIndex i v => Histogram i b v a -> v (Both b a)
bins (Compose (Both f v)) = imap (\i -> Both (f i)) v


_Compose :: Lens' (Compose f g a) (f (g (a)))
_Compose = dimap getCompose Compose 


values :: Traversal' (Histogram i b v a) (v a)
values = _Compose . _2


atH ::  Int -> Traversal' (Histogram Int b IntMap a) (Maybe a)
atH i = values . wander (flip alterF i)


ixH :: Int -> Traversal' (Histogram Int b IntMap a) a
ixH i = atH i . _Just
  where
    _Just = dimap (maybe (Left ()) Right) (either (const Nothing) Just) . right'



-- printDist1D :: Show a => Dist1D a -> Text
-- printDist1D d =
--   T.intercalate "\t" . fmap T.pack
--     $ [ views sumW show d
--       , views sumWW show d
--       , views sumWX (show.fromOnly) d
--       , views sumWXY (show.fromOnly.fromOnly) d
--       , views nentries show d
--       ]
--   where fromOnly (Only x) = x
-- 
-- printDist2D :: Show a => Dist2D a -> Text
-- printDist2D d =
--   T.intercalate "\t" . map T.pack
--     $ [ views sumW show d
--       , views sumWW show d
--       , views (sumWX._1) show d
--       , views (sumWXY._1._1) show d
--       , views (sumWX._2) show d
--       , views (sumWXY._2._2) show d
--       , views nentries show d
--       ]
-- 
-- printBin1D :: Show a => (a, a) -> Text
-- printBin1D (x, y) = showT x <> "\t" <> showT y
-- 
-- printBin2D :: Show a => ((a, a), (a, a)) -> Text
-- printBin2D ((xl, yl), (xh, yh)) =
--   T.intercalate "\t" . fmap showT
--     $ [xl, xh, yl, yh]
-- 
-- showT :: Show a => a -> Text
-- showT = T.pack . show
-- 
-- 
-- printHistogram
--   :: ( IntervalBin b, VG.Vector v Text, VG.Vector v (BinValue b, BinValue b)
--      , VG.Vector v t, Monoid t, Traversable v, Show (BinValue b) )
--   => (t -> Text) -> ((BinValue b, BinValue b) -> Text) -> Histogram v b t -> Text
-- printHistogram showContents showInterval h =
--   T.unlines
--     $ "Total\tTotal\t" <> showContents (total h)
--       : maybe ls (\x -> uo x ++ ls) (view outOfRange h)
-- 
--   where
--     uo (u, o) =
--       [ "Underflow\tUnderflow\t" <> showContents u
--       , "Overflow\tOverflow\t" <> showContents o
--       ]
-- 
--     ls = VG.toList (VG.zipWith f bs (view histData h))
-- 
--     f bi d = showInterval bi <> "\t" <> showContents d
-- 
--     bs = views bins binsList h
