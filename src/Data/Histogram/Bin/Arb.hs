{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Histogram.Bin.Arb
  ( module X
  , ArbBin, arbBin, toArbBin, mergeBinRange
  ) where

import           Control.DeepSeq
import           Data.Histogram.Bin    as X
import           Data.Maybe            (fromMaybe)
import           Data.Serialize
import qualified Data.Vector           as V
import           Data.Vector.Serialize ()
import           GHC.Generics
import           Linear.Epsilon

data ArbBin a = ArbBin Int a (V.Vector a) a
  deriving (Generic, Show)

instance (NFData a) => NFData (ArbBin a) where
instance Serialize a => Serialize (ArbBin a) where

arbBin :: V.Vector a -> ArbBin a
arbBin v = ArbBin (V.length v - 1) (V.head v) v (V.last v)

toArbBin :: IntervalBin b => b -> ArbBin (BinValue b)
toArbBin b = arbBin $ V.snoc (fst <$> bl) (snd . V.last $ bl)
  where
    bl = binsList b

posinf :: Fractional a => a
posinf = 1/0

neginf :: Fractional a => a
neginf = negate posinf

instance (Ord a, Fractional a) => Bin (ArbBin a) where
  type BinValue (ArbBin a) = a

  inRange (ArbBin _ mn _ mx) x = x >= mn && x < mx

  nBins (ArbBin n _ _ _) = n

  toIndex (ArbBin n mn v _) x
    | x < mn = negate 1
    | otherwise = fromMaybe n $ V.findIndex (< x) v

  fromIndex (ArbBin _ _ v _) i
    | i < 0 = neginf
    | otherwise =
      maybe posinf (/2) $ (+) <$> v V.!? i <*> v V.!? (i+1)

instance (Ord a, Fractional a) => IntervalBin (ArbBin a) where
  binInterval (ArbBin _ mn v mx) i
    | i < 0 = (neginf, mn)
    | otherwise = (x, y)
      where
        x = fromMaybe mx $ v V.!? i
        y = fromMaybe posinf $ v V.!? (i+1)

instance (Ord a, Fractional a) => VariableBin (ArbBin a) where
  binSizeN ab i = y - x
    where (x, y) = binInterval ab i

instance (Ord a, Fractional a, Epsilon a)
  => BinEq (ArbBin a) where
  binEq (ArbBin n _ v _) (ArbBin n' _ v' _) =
    (n == n')
      && all nearZero (V.zipWith (-) v v')

-- TODO
-- partial!
mergeBinRange :: Int -> Int -> ArbBin a -> ArbBin a
mergeBinRange k l (ArbBin n mn v mx) =
  if k > l || l >= n
    then error "attempting to merge invalid range of bins."
    else ArbBin (n + k - l) mn (V.slice 0 (k+1) v V.++ V.slice (l+1) (n - l) v) mx
