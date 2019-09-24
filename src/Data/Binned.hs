{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Binned where


import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.Both
import Data.Functor.Compose
import Data.Profunctor.Optic
import Data.Moore
import Data.List (findIndex, intercalate)
import Data.IntMap.Strict as IM
import Data.Gauss
import Data.Functor.Identity


type Binned x = Compose (Both [x]) IM.IntMap

evenBins, evenBins' :: Fractional a => a -> Integer -> a -> [a]

evenBins start num end =
  let diff = (end - start) / fromInteger num
  in (\n -> start + fromInteger n * diff) <$> [0..num+1]

evenBins' start num end = neginf : evenBins start num end ++ [inf]


logBins, logBins' :: Floating a => a -> Integer -> a -> [a]

logBins start num end = exp <$> evenBins (log start) num (log end)

logBins' start num end = neginf : logBins start num end ++ [inf]


pattern Binned :: [x] -> IM.IntMap a -> Binned x a
pattern Binned xs im = Compose (Both xs im)


_Compose :: Lens' (Compose f g a) (f (g (a)))
_Compose = dimap getCompose Compose 

_Binned :: Lens' (Binned x a) (Both [x] (IM.IntMap a))
_Binned = _Compose


inf, neginf :: Fractional a => a
inf = 1/0
neginf = negate inf


-- indexing starts at 0
binIdx0 :: Ord x => [x] -> (x -> Int)
binIdx0 [] _ = -1
binIdx0 xs@(x:_) y =
  if y < x
    then (-1)
    else fromMaybe 0 (findIndex (>= y) xs) - 1


binInterval :: [x] -> (Int -> (x, x))
binInterval xs = (!!) (ranges xs)
  where
    ranges (y:y':ys) = (y, y') : ranges (y':ys)
    ranges [_] = []
    ranges [] = []


binned :: [x] -> [a] -> Binned x a
binned xs vs = Compose . Both xs . IM.fromList $ zip [0 .. length xs - 2] vs


defaultBinned :: [x] -> a -> Binned x a
defaultBinned xs v = binned xs $ repeat v


memptyBinned :: [x] -> Binned x a
memptyBinned xs = binned xs mempty


mooreBinned
  :: Ord x
  => [x] -> Moore' a b -> Moore' (x, a) (Binned x b)
mooreBinned xs m = layerF go $ defaultBinned xs m
  where
    go (x, a) = (a, ixH (binIdx0 xs x))


mooreHisto1D
  :: (Num a, Ord a)
  => [a] -> Moore' (Identity a, a) (Binned a (Gauss Identity a))
mooreHisto1D xs =
  premap (\(Identity v, w) -> (v, (Identity v, w)))
  $ mooreBinned xs mooreGauss


mooreProf1D
  :: (Num a, Ord a)
  => [a] -> Moore' (TF a, a) (Binned a (Gauss TF a))
mooreProf1D xs =
  premap (\(TF x y, w) -> (x, (TF x y, w)))
  $ mooreBinned xs mooreGauss


mooreHisto2D
  :: (Num a, Ord a)
  => [a] -> [a] -> Moore' (TF a, a) (Binned a (Binned a (Gauss TF a)))
mooreHisto2D xs ys =
  premap (\t@(TF x _, _) -> (x, t))
  . mooreBinned xs
  $ mooreProf1D ys


binEdges :: Lens' (Binned x a) [x]
binEdges = _Compose . _1


values :: Lens' (Binned x a) (IM.IntMap a)
values = _Compose . _2


-- TODO
-- this should be a Lens'
atH :: Int -> Traversal' (Binned x a) (Maybe a)
atH i = values . wander (flip alterF i)


ixH :: Int -> Traversal' (Binned x a) a
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
  :: Monoid a
  => (a -> String)
  -> ((b, b) -> String)
  -> Binned b a
  -> String
printBinned showContents showInterval (Compose (Both xs im)) =
  unlines $ "Total\tTotal\t" <> showContents tot : (showBin <$> binconts)

  where
    tot = fold im
    binconts = IM.toAscList im
    showBin (i, d) = showInterval (binInterval xs i) <> "\t" <> showContents d


test
  :: (Fractional b, Ord b)
  => Moore' [(Identity b, b)] (Binned b (Gauss Identity b))
test =
  chomps' [(Identity 2, 1), (Identity 4, -1), (Identity 1, 1), (Identity (-1), 10)]
  . foldlMoore
  $ mooreHisto1D [neginf, 1, 2, 3, inf]


testProf
  :: (Fractional b, Ord b)
  => Moore' [(Identity b, b)] (Binned b (Gauss Identity b))
testProf =
  chomps' [(Identity 2, 1), (Identity 4, -1), (Identity 1, 1), (Identity (-1), 10)]
  . foldlMoore
  $ mooreHisto1D [neginf, 1, 2, 3, inf]

