{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Histogram.Bin.Transform
  ( module X
  , TransformedBin, transformedBin
  , fixedLogBin, fixedLog10Bin
  , BinTransform (..)
  , LogBT, Log10BT
  , Log10Bin, transBinD
  ) where

import           Control.Lens
import           Data.Histogram.Bin       as X
import           Data.Histogram.Bin.Fixed
import           Data.Proxy
import           GHC.Generics             hiding (from)
import           GHC.TypeLits
import           Linear.V                 hiding (Size)

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
  ( BinTransform bt, Fractional (BTDomain bt), IntervalBin b
  , BinValue b ~ BTDomain bt, Ord (BTRange bt) )
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

transBinD
  :: forall t. (BinTransform t, BTDomain t ~ Double, BTRange t ~ Double)
  => Double -> Int -> Double -> TransformedBin BinD t
transBinD mn n mx =
  let f = view . from $ btIso (Proxy :: Proxy t)
  in transformedBin $ binD (f mn) n (f mx)

fixedLogBin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (LogBT a)
fixedLogBin = transformedBin fixedBin

fixedLog10Bin
  :: (KnownNat n, KnownNat min, KnownNat max, Floating a)
  => TransformedBin (FixedBin n min max a) (Log10BT a)
fixedLog10Bin = transformedBin fixedBin

instance Sized (TransformedBin b bt) where
  type Size (TransformedBin b bt) = Size b

type Log10Bin (n :: Nat) (min :: Nat) (max :: Nat) a =
  TransformedBin (FixedBin n min max a) (Log10BT a)
