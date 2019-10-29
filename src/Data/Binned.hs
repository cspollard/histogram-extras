{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell              #-}

module Data.Binned where


import           Both
import           Data.Foldable         (fold)
import           Data.Functor.Compose
import           Data.Gauss
import           Data.StrictIntMap
import qualified Data.IntMap.Strict as IM
import           Data.List             (findIndex, intercalate)
import           Data.Maybe            (fromMaybe)
import           Moore
import Data.Semigroup (First(..))
import Data.Functor.Apply
import Data.Functor.Identity
import Control.Lens
import GHC.Exts (fromList)
import GHC.Generics
import Optic (starry)
import Control.Arrow (arr)
import Data.Serialize
import Data.Histogram.Instances ()


inf, neginf :: Fractional a => a
inf = 1/0
neginf = negate inf


newtype Binned x a
  = Binned { runBinned :: Compose (Both (First [x])) StrictIntMap a }
    deriving (Functor, Foldable, Traversable, Generic)
    deriving (Apply) via (Compose (Both (First [x])) StrictIntMap)


instance (Serialize x, Serialize a) => Serialize (Binned x a) where


type Binned2D x y z = Compose (Binned x) (Binned y) z

binned :: [x] -> StrictIntMap a -> Binned x a
binned xs im = Binned (Compose (Both (First xs) im))

binnedDecomp :: Binned x a -> ([x], StrictIntMap a)
binnedDecomp (Binned (Compose (Both (First xs) im))) = (xs, im)


evenBins, evenBins' :: Fractional a => a -> Integer -> a -> [a]

evenBins start num end =
  let diff = (end - start) / fromInteger num
  in (\n -> start + fromInteger n * diff) <$> [0..num+1]

evenBins' start num end = neginf : evenBins start num end ++ [inf]


logBins, logBins' :: Floating a => a -> Integer -> a -> [a]
logBins start num end = exp <$> evenBins (log start) num (log end)
logBins' start num end = neginf : logBins start num end ++ [inf]



makePrisms ''Binned
makePrisms ''Compose
makePrisms ''First

binEdges :: Lens (Binned x a) (Binned y a) [x] [y]
binEdges = _Binned . _Compose . _1 . _First

binContents :: Lens (Binned x a) (Binned x b) (StrictIntMap a) (StrictIntMap b)
binContents = _Binned . _Compose . _2


-- indexing starts at 0
binIdx0 :: Ord x => [x] -> x -> Int
binIdx0 [] _ = -1
binIdx0 xs@(x:_) y =
  if y < x
    then (-1)
    else fromMaybe 0 (findIndex (>= y) xs) - 1


binInterval :: [x] -> (Int -> (x, x))
binInterval xs = (!!) (ranges xs)
  where
    ranges (y:y':ys) = (y, y') : ranges (y':ys)
    ranges [_]       = []
    ranges []        = []


atBin :: Ord x => Binned x a -> x -> Maybe a
atBin (Binned (Compose (Both (First xs) m))) x =
  view (at $ binIdx0 xs x) m


defaultBinned :: [x] -> a -> Binned x a
defaultBinned xs v = binned xs . fromList $ imap (\i _ -> (i, v)) xs


memptyBinned :: Monoid a => [x] -> Binned x a
memptyBinned xs = defaultBinned xs mempty


mooreBinned
  :: forall x a b. Ord x
  => [x] -> Moore' a b -> Moore' (x, a) (Binned x b)
mooreBinned xs m = simplify . layerF (arr go) $ defaultBinned xs (generalize m)
  where
    go (x, a) = (a, starry $ binContents . ix (binIdx0 xs x))



mooreHisto1D
  :: (Num a, Ord a)
  => [a] -> Moore (->) (Identity a, a) (Binned a (Gauss Identity a))
mooreHisto1D xs =
  premap (\(Identity v, w) -> (v, (Identity v, w)))
  $ mooreBinned xs mooreGauss


mooreProf1D
  :: (Num a, Ord a)
  => [a] -> Moore (->) (TF a, a) (Binned a (Gauss TF a))
mooreProf1D xs =
  premap (\(TF x y, w) -> (x, (TF x y, w)))
  $ mooreBinned xs mooreGauss


mooreHisto2D
  :: (Num a, Ord a)
  => [a] -> [a] -> Moore (->) (TF a, a) (Compose (Binned a) (Binned a) (Gauss TF a))
mooreHisto2D xs ys =
  premap (\t@(TF x _, _) -> (x, t))
  . fmap Compose
  . mooreBinned xs
  $ mooreProf1D ys


printInterval1D :: (Eq a, Fractional a, Show a) => (a, a) -> String
printInterval1D (x, y) =
  case (x == neginf, y == inf) of
    (True, _) -> "Underflow\tUnderflow"
    (_, True) -> "Overflow\tOverflow"
    _         -> show x <> "\t" <> show y


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
printBinned showContents showInterval b =
  unlines $ "Total\tTotal\t" <> showContents tot : (showBin <$> binconts)

  where
    (xs, im) = binnedDecomp b
    tot = fold im
    binconts = inSIM IM.toAscList im
    showBin (i, d) = showInterval (binInterval xs i) <> "\t" <> showContents d
