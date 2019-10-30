{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell              #-}

module Data.Binned where


import           Both
import           Data.Functor.Compose
import           Data.Gauss
import           Data.StrictIntMap
import qualified Data.IntMap.Strict as IM
import           Data.List             (findIndex)
import           Data.Maybe            (fromMaybe, catMaybes)
import           Moore
import Data.Semigroup (Last(..))
import Data.Functor.Apply
import Data.Functor.Identity
import Control.Lens
import GHC.Exts (fromList)
import GHC.Generics
import Optic (starry)
import Control.Arrow (arr)
import Data.Serialize
import Data.Histogram.Instances ()
import Data.Foldable (fold)


inf, neginf :: Fractional a => a
inf = 1/0
neginf = negate inf


newtype Binned x a
  = B { runB :: Compose (Both (Last [x])) StrictIntMap a }
    deriving (Functor, Foldable, Traversable, Generic)
    deriving (Apply) via (Compose (Both (Last [x])) StrictIntMap)
    deriving (Semigroup) via (Both (Last [x]) (StrictIntMap a))

pattern Binned :: [x] -> StrictIntMap a -> Binned x a
pattern Binned xs sm = B (Compose (Both (Last xs) sm))


instance (Serialize a) => Serialize (Last a) where
instance (Serialize x, Serialize a) => Serialize (Binned x a) where


type Binned2D x y z = Compose (Binned x) (Binned y) z

binned :: [x] -> StrictIntMap a -> Binned x a
binned = Binned


evenBins, evenBins' :: Fractional a => a -> Integer -> a -> [a]

evenBins start num end =
  let diff = (end - start) / fromInteger num
  in (\n -> start + fromInteger n * diff) <$> [0..num]

evenBins' start num end = neginf : evenBins start num end ++ [inf]


logBins, logBins' :: Floating a => a -> Integer -> a -> [a]
logBins start num end = exp <$> evenBins (log start) num (log end)
logBins' start num end = neginf : logBins start num end ++ [inf]



makePrisms ''Binned
makePrisms ''Compose
makePrisms ''Last

binEdges :: Lens (Binned x a) (Binned y a) [x] [y]
binEdges = _B . _Compose . _1 . _Last


binContents :: Lens (Binned x a) (Binned x b) (StrictIntMap a) (StrictIntMap b)
binContents = _B . _Compose . _2


-- indexing starts at 0
binIdx0 :: Ord x => [x] -> x -> Int
binIdx0 [] _ = -1
binIdx0 xs@(x:_) y =
  if y < x
    then (-1)
    else fromMaybe 0 (findIndex (>= y) xs) - 1


binRanges :: [a] -> [(a, a)]
binRanges (y:y':ys) = (y, y') : binRanges (y':ys)
binRanges [_]       = []
binRanges []        = []


atBin :: Ord x => Binned x a -> x -> Maybe a
atBin (Binned xs m) x = view (at $ binIdx0 xs x) m


defaultBinned :: [x] -> a -> Binned x a
defaultBinned [] _ = binned [] mempty
defaultBinned xs v = binned xs . fromList . imap (\i _ -> (i, v)) $ tail xs


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
printInterval2D :: (Show a, Eq a, Fractional a) => (a, a) -> Maybe String
printInterval2D (xl, xh) =
  case (xl == neginf, xh == inf) of
    (True, _) -> Nothing
    (_, True) -> Nothing
    _         -> Just $ show xl <> "\t" <> show xh


binList :: Binned b a -> [((b, b), a)]
binList (Binned xs im) = zip ranges binconts
  where
    binconts = snd <$> inSIM IM.toAscList im
    ranges = binRanges xs



printBinned1D
  :: (Monoid a, Eq b, Fractional b, Show b)
  => (a -> String)
  -> Binned b a
  -> String
printBinned1D printContents b =
  unlines $ "Total\tTotal\t" <> printContents tot : (printBin <$> bl)

  where
    bl = binList b
    tot = fold b
    printBin (bi, d) = printInterval1D bi <> "\t" <> printContents d


printHisto1D :: Binned Double (Gauss1D Double) -> String
printHisto1D = printBinned1D printGauss1D


printBinned2D
  :: (Monoid a, Eq b, Fractional b, Show b, Eq c, Fractional c, Show c)
  => (a -> String)
  -> Compose (Binned b) (Binned c) a
  -> String
printBinned2D printContents b =
  unlines . catMaybes
  $ Just ("Total\tTotal\t" <> printContents tot)
    : (printBin <$> bl)

  where
    Compose b' = b

    bl = do
      (xbinedges, ybin) <- binList b'
      (ybinedges, val) <- binList ybin
      return ((xbinedges, ybinedges), val)

    tot = fold b
    printBin ((bx, by), d) =
      mconcat <$> sequenceA [printInterval2D bx, printInterval2D by, Just $ printContents d]
