{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Dist
  ( DistND(..), sumW, sumWW, sumWX, sumWXY, nentries
  , Dist0D, Dist1D, Dist2D
  ) where

import           Control.Lens
import           Data.Serialize
import qualified Data.Vector.Unboxed            as U
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import           GHC.TypeLits
import           Linear.V
import           Linear.Vector

import           Data.Fillable
import           Data.Histogram.Internal.TriMat
import           Data.Weighted


fillV :: (KnownNat n, Num a) => a -> V n a -> V n a -> V n a
fillV w v v' = (w *^ v) ^+^ v'


data DistND n a =
  DistND
    { _sumW     :: !a
    , _sumWW    :: !a
    , _sumWX    :: !(V n a)
    , _sumWXY   :: !(TriMat n a)
    , _nentries :: !Int
    } deriving Generic

instance (KnownNat n, KnownNat (Tri n), Serialize a)
  => Serialize (DistND n a) where

type Dist0D a = DistND 0 a
type Dist1D a = DistND 1 a
type Dist2D a = DistND 2 a

makeLenses ''DistND

instance
  ( KnownNat n, Monoid a, Monoid (TriMat n a) )
  => Monoid (DistND n a) where

  DistND sw sww swx swxy n `mappend` DistND sw' sww' swx' swxy' n' =
    DistND
      (sw `mappend` sw')
      (sww `mappend` sww')
      (mappend <$> swx <*> swx')
      (swxy `mappend` swxy')
      (n+n')

  mempty = DistND mempty mempty (pure mempty) mempty 0


derivingUnbox "DistND"
  [t| forall a n. (KnownNat n, U.Unbox a, U.Unbox (TriMat n a)) => DistND n a -> (a, a, V n a, TriMat n a, Int) |]
  [| \(DistND sw sww swx swxy n) -> (sw, sww, swx, swxy, n) |]
  [| \(sw, sww, swx, swxy, n) -> DistND sw sww swx swxy n |]


instance (Functor (TriMat n), Fractional a) => Weighted (DistND n a) where
  type Weight (DistND n a) = a
  scaling w (DistND sw sww swx swxy n) =
    DistND
      (sw*w)
      (sww*w*w)
      (w *^ swx)
      ((*w) <$> swxy)
      n

  integral =
    lens
    (view sumW)
    (\d w -> let wr = w / view sumW d in scaling wr d)


-- TODO
-- this is kind of inelegant
-- it loops explicitly over indices...
fillCov
  :: (KnownNat (Tri n), KnownNat n, Num a)
  => a -> V n a -> TriMat n a -> TriMat n a
fillCov w v = imap $ \(i, j) x -> x + w*(v ^?! ix i)*(v ^?! ix j)

instance (KnownNat n, KnownNat (Tri n), Functor (TriMat n), Fractional a)
  => Fillable (DistND n a) where
    type FillVec (DistND n a) = V n a
    filling v w (DistND sw sww swx swxy n) =
      DistND (sw+w) (sww+(w*w)) (fillV w v swx) (fillCov w v swxy) (n+1)
