{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Histogram.Bin.Fixed
  ( module X
  , FixedBin, fixedBin
  , Sized(..)
  , IntBin
  ) where

import           Data.Proxy
import           Data.Serialize
import           Data.Vector.Serialize      ()
import           GHC.Generics               hiding (from)
import           GHC.TypeLits
import           Linear.V                   hiding (Size)

import           Data.Histogram.Bin.Classes as X

data FixedBin :: (n -> a -> b -> * -> *) where
  FixedBin :: c -> c -> c -> FixedBin n a b c
  deriving (Generic, Show)

instance Serialize c => Serialize (FixedBin n a b c) where

fixedBin
  :: forall n minT maxT a.
    ( Fractional a, KnownNat n, KnownNat minT, KnownNat maxT )
  => FixedBin n minT maxT a
fixedBin = FixedBin mn step mx
  where
    mn = fromIntegral . natVal $ (Proxy :: Proxy minT)
    mx = fromIntegral . natVal $ (Proxy :: Proxy maxT)
    step = (mx - mn) / fromIntegral (natVal (Proxy :: Proxy n))



instance (KnownNat n, RealFrac a) => Bin (FixedBin n min max a) where
  type BinValue (FixedBin n min max a) = a

  inRange (FixedBin mn _ mx) x = x >= mn && x < mx

  nBins _ = fromIntegral $ natVal (Proxy :: Proxy n)

  toIndex fb@(FixedBin mn step mx) x
    | x < mn = negate 1
    | x > mx = nBins fb
    | otherwise = floor $ (x - mn) / step

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



class Sized s where
  type Size s :: k

instance Sized (FixedBin n min max a) where
  type Size (FixedBin n min max a) = n

type IntBin (min :: Nat) (max :: Nat) a = FixedBin (max - min) min max a
