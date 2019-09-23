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


histMoore1D
  :: (Fractional x, Ord x)
  => [x] -> (Moore' a b) -> Moore' (x, a) (Binned Int (x, x) IM.IntMap b)
histMoore1D xs m =
  let Both idx range = binning xs
  in layerF
      (\(x, a) -> (a, ixH (idx x)))
      (binned range (IM.fromList $ zip [0 .. length xs] (repeat $ m)))


-- histMoore2D
  -- :: (Fractional x, Ord x)
  -- => [x] -> [y] -> Moore' ((x, y), a) (Binned Int (x, x) IM.IntMap (Binned Int (y, y) IM.IntMap Int))
-- histMoore2D xs =
  -- let Both idx range = binning xs
  -- in layerF
      -- (\(x, a) -> (a, ixH (idx x)))
      -- (binned range (IM.fromList $ zip [0 .. length xs] (repeat $ counter 0)))


bins :: FunctorWithIndex i v => Binned i b v a -> v (b, a)
bins (Compose (Both f v)) = imap (\i -> (f i,)) v


_Compose :: Lens' (Compose f g a) (f (g (a)))
_Compose = dimap getCompose Compose 


values :: Lens' (Binned i b v a) (v a)
values = _Compose . _2


atH ::  Int -> Traversal' (Binned Int b IntMap a) (Maybe a)
atH i = values . wander (flip alterF i)


ixH :: Int -> Traversal' (Binned Int b IntMap a) a
ixH i = atH i . _Just
  where
    _Just = dimap (maybe (Left ()) Right) (either (const Nothing) Just) . right'



printBin1D :: (Eq a, Fractional a, Show a) => (a, a) -> String
printBin1D (x, y) =
  case (x == neginf, y == inf) of
    (True, _) -> "Underflow\tUnderflow"
    (_, True) -> "Overflow\tOverflow"
    _ -> show x <> "\t" <> show y


-- TODO
-- this does not handle overflows
printBin2D :: Show a => ((a, a), (a, a)) -> String
printBin2D ((xl, yl), (xh, yh)) = intercalate "\t" $ show <$> [xl, xh, yl, yh]


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
  :: (Fractional a, Ord a, Enum b, Num b)
  => Moore (->) [(a, ())] (Binned Int (a, a) IntMap b)
test =
  chomps' [(2, ()), (4, ()), (1, ())] . foldlMoore
  $ histMoore1D [1, 2, 3] (counter 0)
