{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Histogram.Internal.TriMat where

import           Control.Lens
import           Data.Proxy
import           Data.Serialize
import qualified Data.Vector                  as Vec
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import           GHC.TypeLits
import           Linear.V

type family Tri (n :: Nat) :: Nat where
  Tri 0 = 0
  Tri n = n + Tri (n-1)

newtype TriMat (n :: Nat) a = TriMat { unTM :: V (Tri n) a }
  deriving (Functor, Foldable, Traversable, Generic)

instance (KnownNat n, KnownNat (Tri n), Serialize a)
  => Serialize (TriMat n a) where

instance (KnownNat (Tri n)) => Applicative (TriMat n) where
  pure = TriMat . pure
  TriMat v <*> TriMat v' = TriMat $ v <*> v'

derivingUnbox "TriMat"
  [t| forall n a. (KnownNat (Tri n), U.Unbox a) => TriMat n a -> V (Tri n) a |]
  [| unTM |]
  [| TriMat |]

type instance Index (TriMat n a) = (Int, Int)
type instance IxValue (TriMat n a) = a

-- TODO
-- this is wrong.
-- instance KnownNat n => Ixed (TriMat n a) where
--   ix (i, j) f (TriMat v) =
--     let s = fromIntegral $ natVal (Proxy :: Proxy n)
--     in case () of
--       _ | i >= s -> pure $ TriMat v
--       _ | j > i -> pure $ TriMat v
--       _ -> TriMat <$> ix (i+j) f v

-- idxToCell :: Int -> Int -> Int -> Int
-- idxToCell n i j = (n*(n-1) `mod` 2) - ((n-i)*(n-i-1) `mod` 2) + j - i - 1


triIdcs :: forall n. KnownNat n => TriMat n (Int, Int)
triIdcs =
  let s = fromIntegral $ natVal (Proxy :: Proxy n)
  in TriMat . V
      $ foldMap (\x -> (x,) <$> Vec.enumFromN 0 x) $ Vec.enumFromN 0 (s-1)
{-# INLINABLE triIdcs #-}

instance (KnownNat n, KnownNat (Tri n)) => FunctorWithIndex (Int, Int) (TriMat n) where
instance (KnownNat n, KnownNat (Tri n)) => FoldableWithIndex (Int, Int) (TriMat n) where
instance (KnownNat n, KnownNat (Tri n)) => TraversableWithIndex (Int, Int) (TriMat n) where
  itraverse f (TriMat v) =
    let (TriMat vIdx) = triIdcs :: (TriMat n (Int, Int))
    in fmap TriMat . sequenceA $ f <$> vIdx <*> v
