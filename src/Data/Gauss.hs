{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Gauss
  ( Gauss(..), sumW, sumWW, sumWX, sumWXY, nentries
  , Gauss0D, Gauss1D, Gauss2D
  , fillGauss, mooreGauss
  , printGauss1D, printGauss2D
  ) where


import Control.Lens
import Data.Proxy
import Linear.Vector
import Linear.Matrix
import Both
import Data.Serialize
import GHC.Generics
import Data.List (intercalate)
import Data.Biapplicative
import Data.Bifunctor.Join
import Data.Functor.Identity
import Moore


data Gauss v a
  = Gauss
  { _sumW     :: !a
  , _sumWW    :: !a
  , _sumWX    :: !(v a)
  , _sumWXY   :: !(v (v a))
  , _nentries :: !Int
  } deriving (Generic, Functor)


makeLenses ''Gauss


instance (Serialize a, Serialize (v a), Serialize (v (v a)))
  => Serialize (Gauss v a) where


type Gauss0D = Gauss Proxy
type Gauss1D = Gauss Identity
type Gauss2D = Gauss TF


instance Additive v => Additive (Gauss v) where
  zero = Gauss 0 0 zero (outer zero zero) 0

  liftI2 f (Gauss sw sww swx swxy n) (Gauss sw' sww' swx' swxy' n') =
    Gauss (f sw sw') (f sww sww') (liftI2 f swx swx') (liftI2 (liftI2 f) swxy swxy') (n + n')

  liftU2 = liftI2

  (^+^) = liftU2 (+)

  Gauss sw sww swx swxy n ^-^ Gauss sw' sww' swx' swxy' n' =
    Gauss (sw - sw') (sww - sww') (swx ^-^ swx') (swxy !-! swxy') (n - n')


instance (Additive v, Num a) => Semigroup (Gauss v a) where
  (<>) = (^+^)

instance (Additive v, Num a) => Monoid (Gauss v a) where
  mempty = zero


instance Biapplicative p => Additive (Join p) where
  zero = pure 0


fillGauss :: (Additive v, Num a) => Gauss v a -> (v a, a) -> Gauss v a
fillGauss (Gauss sw sww swx swxy n) (v, w) =
  Gauss
    (sw + w)
    (sww + w * w)
    (w *^ v ^+^ swx)
    (outer v (w *^ v) !+! swxy)
    (n + 1)


mooreGauss :: (Additive v, Num a) => Moore' (v a, a) (Gauss v a)
mooreGauss = feedback $ liftMoore zero (uncurry fillGauss)


printGauss1D :: Show a => Gauss1D a -> String
printGauss1D d =
  intercalate "\t"
  [ views sumW show d
  , views sumWW show d
  , views sumWX (show.runIdentity) d
  , views sumWXY (show.runIdentity.runIdentity) d
  , views nentries show d
  ]


printGauss2D :: Show a => Gauss2D a -> String
printGauss2D d =
  intercalate "\t"
  $ [ views sumW show d
  , views sumWW show d
  , views (sumWX . true) show d
  , views (sumWXY . true . true) show d
  , views (sumWX . false) show d
  , views (sumWXY . false . false) show d
  , views nentries show d
  ]
