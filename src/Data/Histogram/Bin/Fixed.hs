{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram.Bin.Fixed
  ( module X
  , FixedBin, fixedBin
  , ArbBin, arbBin, toArbBin
  , TransformedBin, transformedBin
  , fixedLogBin, fixedLog10Bin
  , BinTransform (..)
  , LogBT, Log10BT
  , Sized(..)
  , Log10Bin, IntBin
  ) where

import Data.Vector.Serialize ()
import Data.Serialize
import GHC.Generics hiding (from)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Linear.V
import           Linear.Epsilon
import Data.List (intersperse)
import Control.Lens
import Data.Proxy
import GHC.TypeLits

import Data.Histogram.Bin.Classes as X

data FixedBin :: (n -> a -> b -> * -> *) where
  FixedBin :: c -> c -> c -> FixedBin n a b c
  deriving (Generic, Show)

instance Serialize c => Serialize (FixedBin n a b c) where

instance (KnownNat n, Show c) => Show (FixedBin n min max c) where
  show (FixedBin mn _ mx) =
    mconcat . intersperse " " $ ["FixedBin", n', mn', mx']
    where
      n' = show (natVal (Proxy :: Proxy n))
      mn' = show mn
      mx' = show mx

fixedBin
  :: forall n min max a. (Fractional a, KnownNat n, KnownNat min, KnownNat max)
  => FixedBin n min max a
fixedBin = FixedBin mn step mx
  where
    mn = fromIntegral . natVal $ (Proxy :: Proxy min)
    mx = fromIntegral . natVal $ (Proxy :: Proxy max)
    step = (mx-mn) / fromIntegral (natVal (Proxy :: Proxy n))



instance (KnownNat n, RealFrac a) => Bin (FixedBin n min max a) where
  type BinValue (FixedBin n min max a) = a

  inRange (FixedBin mn _ mx) x = x >= mn && x < mx

  nBins _ = fromIntegral $ natVal (Proxy :: Proxy n)

  toIndex fb@(FixedBin mn step mx) x
    | x < mn = negate 1
    | x > mx = nBins fb
    | otherwise = floor $ (x-mn) / step

  fromIndex (FixedBin mn step _) i =
    mn + (fromIntegral i + 0.5) * step


instance (KnownNat n, RealFrac a) => IntervalBin (FixedBin n min max a) where
  binInterval (FixedBin mn step _) i = (x, x+step)
    where x = mn + fromIntegral i * step

instance (KnownNat n, RealFrac a) => VariableBin (FixedBin n min max a) where
  binSizeN (FixedBin _ step _) _ = step

instance (KnownNat n, RealFrac a) => UniformBin (FixedBin n min max a) where
  binSize (FixedBin _ step _) = step

instance (KnownNat n, RealFrac a) => BinEq (FixedBin n min max a) where
  binEq _ _ = True

instance KnownNat n => Dim (FixedBin n min max a) where
  reflectDim _ = fromIntegral $ natVal (Proxy :: Proxy n)


data ArbBin a = ArbBin Int a (V.Vector a) a
  deriving (Generic, Show)

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


class BinTransform bt where
  type BTDomain bt :: *
  type BTRange bt :: *
  btIso :: Proxy bt -> Iso' (BTDomain bt) (BTRange bt)

safeLog :: (Ord t, Floating t) => t -> t
safeLog x
  | x <= 0 = log 0
  | otherwise = log x

data LogBT a
  deriving Generic

instance (Ord a, Floating a) => BinTransform (LogBT a) where
  type BTDomain (LogBT a) = a
  type BTRange (LogBT a) = a
  btIso _ = iso exp safeLog

safeLog10 :: (Ord t, Floating t) => t -> t
safeLog10 x
  | x <= 0 = log 0
  | otherwise = logBase 10 x

data Log10BT a
  deriving Generic

instance (Ord a, Floating a) => BinTransform (Log10BT a) where
  type BTDomain (Log10BT a) = a
  type BTRange (Log10BT a) = a
  btIso _ = iso (10**) safeLog10

data TransformedBin b bt = TransformedBin b (Proxy bt)
  deriving (Generic, Show)

transformedBin :: b -> TransformedBin b bt
transformedBin b = TransformedBin b (Proxy :: Proxy bt)

instance
  ( BinTransform bt, Fractional (BTDomain bt), IntervalBin b
  , BinValue b ~ BTDomain bt )
  => Bin (TransformedBin b bt) where
  type BinValue (TransformedBin b bt) = BTRange bt

  inRange (TransformedBin b p) = views (from $ btIso p) (inRange b)

  nBins (TransformedBin b _) = nBins b

  toIndex (TransformedBin b p) = views (from $ btIso p) (toIndex b)

  fromIndex (TransformedBin b p) i = view (btIso p) x
    where x = (/2) . uncurry (+) $ binInterval b i


instance
  (BinTransform bt, Fractional (BTDomain bt), IntervalBin b, BinValue b ~ BTDomain bt, Ord (BTRange bt))
  => IntervalBin (TransformedBin b bt) where
  binInterval (TransformedBin b p) j =
    let i = btIso p
    in bimap (view i) (view i) $ binInterval b j


instance
  ( BinTransform bt, Fractional (BTDomain bt), IntervalBin b
  , BinValue b ~ BTDomain bt, Num (BTRange bt), Ord (BTRange bt) )
  => VariableBin (TransformedBin b bt) where
  binSizeN tb i = uncurry (flip (-)) $ binInterval tb i

instance
  ( BinTransform bt, Fractional (BTDomain bt), IntervalBin b
  , BinValue b ~ BTDomain bt )
  => BinEq (TransformedBin b bt) where
  binEq _ _ = True

instance (Dim b) => Dim (TransformedBin b bt) where
  reflectDim _ = reflectDim (Proxy :: Proxy b)


fixedLogBin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (LogBT a)
fixedLogBin = transformedBin fixedBin

fixedLog10Bin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (Log10BT a)
fixedLog10Bin = transformedBin fixedBin


class Sized s where
  type Size s :: k

instance Sized (FixedBin n min max a) where
  type Size (FixedBin n min max a) = n

instance Sized (TransformedBin b bt) where
  type Size (TransformedBin b bt) = Size b

type IntBin (min :: Nat) (max :: Nat) a = FixedBin (max-min) min max a
type Log10Bin (n :: Nat) (min :: Nat) (max :: Nat) a =
  TransformedBin (FixedBin n min max a) (Log10BT a)
