{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Dist
  ( DistND(..), sumW, sumWW, sumWX, sumWXY, nentries
  , Dist0D, Dist1D, Dist2D
  , Pair(..), Only(..), Empty(..)
  , removeSubDist
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Fillable
import           Data.Histogram.Instances     ()
import           Data.Semigroup
import           Data.Serialize
import           Data.Vector.Fixed            as V
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving
import           Data.Weighted
import           GHC.Generics                 hiding (S, V1)


data DistND v a =
  DistND
    { _sumW     :: {-# UNPACK #-} !a
    , _sumWW    :: {-# UNPACK #-} !a
    , _sumWX    :: {-# UNPACK #-} !(v a)
    , _sumWXY   :: {-# UNPACK #-} !(v (v a))
    , _nentries :: {-# UNPACK #-} !Int
    } deriving Generic

makeLenses ''DistND

instance (NFData a, NFData (v a), NFData (v (v a))) => NFData (DistND v a) where

instance (Serialize a, Serialize (v a), Serialize (v (v a)))
  => Serialize (DistND v a) where

type Dist0D a = DistND Empty a
type Dist1D a = DistND Only a
type Dist2D a = DistND Pair a

-- strict tuple type
data Pair a =
  Pair
    {-# UNPACK #-} !a
    {-# UNPACK #-} !a
    deriving (Generic, Show)

instance NFData a => NFData (Pair a) where
instance Serialize a => Serialize (Pair a) where

instance Field1 (Pair a) (Pair a) a a where
  _1 f (Pair x y) = (`Pair` y) <$> f x

instance Field2 (Pair a) (Pair a) a a where
  _2 f (Pair x y) = Pair x <$> f y

type instance Dim Pair = S (S Z)
instance V.Vector Pair a where
  construct = Fun Pair
  inspect (Pair x y) (Fun f) = f x y


instance (Num a, Vector v a, Vector v (v a)) => Semigroup (DistND v a) where
  DistND sw sww swx swxy n <> DistND sw' sww' swx' swxy' n' =
    DistND
      (sw + sw')
      (sww + sww')
      (V.zipWith (+) swx swx')
      (V.zipWith (V.zipWith (+)) swxy swxy')
      (n + n')

instance (Num a, Vector v a, Vector v (v a)) => Monoid (DistND v a) where
  mappend = (<>)
  mempty =
    DistND 0 0 (V.replicate 0) (V.replicate $ V.replicate 0) 0


-- a fully correlated difference between dists.
removeSubDist
  :: (Num a, Vector v a, Vector v (v a))
  => DistND v a -> DistND v a -> DistND v a
removeSubDist (DistND sw sww swx swxy n) (DistND sw' sww' swx' swxy' n') =
  DistND
    (sw - sw')
    (sww - sww')
    (V.zipWith (-) swx swx')
    (V.zipWith (V.zipWith (-)) swxy swxy')
    (n - n')

derivingUnbox "Pair"
  [t| forall a. U.Unbox a => Pair a -> (a, a) |]
  [| \(Pair x y) -> (x, y) |]
  [| uncurry Pair |]

derivingUnbox "DistND"
  [t| forall a v. (U.Unbox a, U.Unbox (v a), U.Unbox (v (v a))) => DistND v a -> (a, a, v a, v (v a), Int) |]
  [| \(DistND sw sww swx swxy n) -> (sw, sww, swx, swxy, n) |]
  [| \(sw, sww, swx, swxy, n) -> DistND sw sww swx swxy n |]


instance (Vector v a, Vector v (v a), Fractional a) => Weighted (DistND v a) where
  type Weight (DistND v a) = a
  scaling w (DistND sw sww swx swxy n) =
    DistND
      (sw * w)
      (sww * w * w)
      (V.map (*w) swx)
      (V.map (V.map (*w)) swxy)
      n

  integral =
    lens
    (view sumW)
    (\d w -> let wr = w / view sumW d in scaling wr d)


outerV :: (Num a, Vector v a, Vector v (v a)) => v a -> v a -> v (v a)
outerV v v' = V.generate (\i -> V.map (* (v V.! i)) v')

instance (Vector v a, Vector v (v a), Fractional a) => Fillable (DistND v a) where
    type FillVec (DistND v a) = v a
    filling v w (DistND sw sww swx swxy n) =
      DistND
        (sw + w)
        (sww + w * w)
        (V.zipWith (+) (V.map (*w) v) swx)
        (V.zipWith (V.zipWith (+)) (outerV v $ V.map (*w) v) swxy)
        (n + 1)
