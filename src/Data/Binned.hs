{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Binned where


import Analysis.Fold
import Data.Foldable (fold)
import Data.Both
import Data.Functor.Compose
import Data.Profunctor.Optic
import Data.Moore
import Data.List (findIndex, intercalate)
import Data.IntMap.Strict as IM
import Control.Lens (FunctorWithIndex(..))
import Data.Gauss
import Data.Functor.Identity


type Binned i b = Compose (Both (i -> b))


pattern Binned :: (i -> b) -> v a -> Binned i b v a
pattern Binned f v = Compose (Both f v)


binned :: FunctorWithIndex i v => (i -> b) -> v a -> Binned i b v a
binned f = Compose . Both f


inf, neginf :: Fractional a => a
inf = 1/0
neginf = negate inf


binning :: (Fractional x, Ord x) => [x] -> Both (x -> Int) (Int -> (x, x))
binning xs = Both go go'
  where
    go x =
      maybe
        (error "in Data.Hist: we should always find a bin index.")
        (\y -> y - 1)
        $ findIndex (> x) l1

    go' i = l2 !! i

    ranges (y:y':ys) = (y, y') : ranges (y':ys)
    ranges [_] = []
    ranges [] = []

    l1 = (neginf : xs) ++ pure inf

    l2 = ranges l1


mooreBinned1D
  :: (Fractional x, Ord x)
  => [x] -> Moore' a b -> Moore' (x, a) (Binned Int (x, x) IM.IntMap b)
mooreBinned1D xs m =
  let Both idx range = binning xs
  in layerF
      (\(x, a) -> (a, ixH (idx x)))
      $ binned range (IM.fromList $ zip [0 .. length xs] (repeat $ m))


mooreBinned2D
  :: (Fractional x, Ord x)
  => [y]
  -> [x]
  -> Moore' a b
  -> Moore' (y, (x, a)) (Binned Int (y, y) IM.IntMap (Binned Int (x, x) IM.IntMap b))
mooreBinned2D xs ys =
  let Both xidx xrange = binning xs
      Both yidx yrange = binning ys
  in layerF
      (\((x, y) a) -> ((y, a), ixH (idx x)))
      (binned range (IM.fromList $ zip [0 .. length xs] (repeat $ counter 0)))


mooreHisto1D
  :: (Fractional a, Ord a)
  => [a] -> Moore' (Identity a, a) (Binned Int (a, a) IntMap (Gauss Identity a))
mooreHisto1D xs =
  premap (\(Identity v, w) -> (v, (Identity v, w))) $ mooreBinned1D xs mooreGauss



bins :: FunctorWithIndex i v => Binned i b v a -> v (b, a)
bins (Compose (Both f v)) = imap (\i -> (f i,)) v


_Binned, _Compose :: Lens' (Compose f g a) (f (g (a)))
_Compose = dimap getCompose Compose 

_Binned = _Compose


values :: Lens' (Binned i b v a) (v a)
values = _Compose . _2


atH ::  Int -> Traversal' (Binned Int b IntMap a) (Maybe a)
atH i = values . wander (flip alterF i)


ixH :: Int -> Traversal' (Binned Int b IntMap a) a
ixH i = atH i . _Just
  where
    _Just = dimap (maybe (Left ()) Right) (either (const Nothing) Just) . right'



printInterval1D :: (Eq a, Fractional a, Show a) => (a, a) -> String
printInterval1D (x, y) =
  case (x == neginf, y == inf) of
    (True, _) -> "Underflow\tUnderflow"
    (_, True) -> "Overflow\tOverflow"
    _ -> show x <> "\t" <> show y


-- TODO
-- this does not handle overflows
printInterval2D :: Show a => ((a, a), (a, a)) -> String
printInterval2D ((xl, yl), (xh, yh)) = intercalate "\t" $ show <$> [xl, xh, yl, yh]


printBinned
  :: (Foldable v, FunctorWithIndex i v, Monoid a)
  => (a -> String)
  -> (b -> String)
  -> (v (b, a) -> [(b, a)])
  -> Binned i b v a
  -> String
printBinned showContents showInterval toList' b =
  unlines $ "Total\tTotal\t" <> showContents tot : (uncurry showBin <$> binconts)

  where
    tot = fold $ view values b
    binconts = toList' $ bins b
    showBin bi d = showInterval bi <> "\t" <> showContents d


test
  :: (Fractional b, Ord b)
  => Moore' [(Identity b, b)] (Binned Int (b, b) IntMap (Gauss Identity b))
test =
  chomps' [(2, 1), (4, -1), (1, 1)] . foldlMoore
  $ mooreHisto1D [1, 2, 3]
