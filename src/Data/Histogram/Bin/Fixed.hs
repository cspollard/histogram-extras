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

module Data.Histogram.Bin.Fixed
  ( module X
  , FixedBin, fixedBin
  , TransformedBin (..)
  , fixedLogBin, fixedLog10Bin
  , BinTransform (..)
  , LogBT, Log10BT
  ) where

import Data.List (intersperse)
import Control.Lens
import Data.Proxy
import GHC.TypeLits

import Data.Histogram.Bin.Classes as X

data FixedBin :: (n -> a -> b -> * -> *) where
  FixedBin :: c -> c -> c -> FixedBin n a b c

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
    mn + fromIntegral i * step


instance (KnownNat n, RealFrac a) => IntervalBin (FixedBin n min max a) where
  binInterval fb@(FixedBin _ step _) i = (x, x+step)
    where x = fromIndex fb i

instance (KnownNat n, RealFrac a) => VariableBin (FixedBin n min max a) where
  binSizeN (FixedBin _ step _) _ = step

instance (KnownNat n, RealFrac a) => UniformBin (FixedBin n min max a) where
  binSize (FixedBin _ step _) = step



class BinTransform bt where
  type BTDomain bt :: *
  type BTRange bt :: *
  btIso :: Proxy bt -> Iso' (BTDomain bt) (BTRange bt)

safeLog :: (Ord t, Floating t) => t -> t
safeLog x
  | x <= 0 = log 0
  | otherwise = log x

data LogBT a

instance (Ord a, Floating a) => BinTransform (LogBT a) where
  type BTDomain (LogBT a) = a
  type BTRange (LogBT a) = a
  btIso _ = iso exp safeLog

safeLog10 :: (Ord t, Floating t) => t -> t
safeLog10 x
  | x <= 0 = log 0
  | otherwise = logBase 10 x

data Log10BT a

instance (Ord a, Floating a) => BinTransform (Log10BT a) where
  type BTDomain (Log10BT a) = a
  type BTRange (Log10BT a) = a
  btIso _ = iso (10**) safeLog10

data TransformedBin b bt = TransformedBin b (Proxy bt)

transFixedBin
  :: (KnownNat n, KnownNat min, KnownNat max, Fractional a)
  => TransformedBin (FixedBin n min max a) bt
transFixedBin = TransformedBin fixedBin Proxy

instance (BinTransform bt, Bin b, BinValue b ~ BTDomain bt)
  => Bin (TransformedBin b bt) where
  type BinValue (TransformedBin b bt) = BTRange bt

  inRange (TransformedBin fb p) = views (from $ btIso p) (inRange fb)

  nBins (TransformedBin fb _) = nBins fb

  toIndex (TransformedBin fb p) = views (from $ btIso p) (toIndex fb)

  fromIndex (TransformedBin fb p) = view (btIso p) . fromIndex fb


instance (BinTransform bt, IntervalBin b, BinValue b ~ BTDomain bt, Ord (BTRange bt))
  => IntervalBin (TransformedBin b bt) where
  binInterval (TransformedBin b p) j =
    let i = btIso p
    in bimap (view i) (view i) $ binInterval b j


instance
  ( BinTransform bt, IntervalBin b, BinValue b ~ BTDomain bt
  , Num (BTRange bt), Ord (BTRange bt) )
  => VariableBin (TransformedBin b bt) where
  binSizeN tb i = uncurry (flip (-)) $ binInterval tb i

fixedLogBin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (LogBT a)
fixedLogBin = transFixedBin

fixedLog10Bin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (Log10BT a)
fixedLog10Bin = transFixedBin

{-
class KnownRat r where
  ratVal :: Proxy r -> Rational

data Rat (m :: a) (n :: a)

infixl 7 /
type family (m :: Nat) / (n :: Nat)

instance KnownNat n => KnownRat n where
  ratVal _ = fromIntegral . natVal $ (Proxy :: Proxy n)

instance (KnownNat n, KnownNat m) => KnownRat (Rat m n) where
  ratVal _ =
    let n' = natVal (Proxy :: Proxy n)
        m' = natVal (Proxy :: Proxy m)
    in n' % m'

-}
