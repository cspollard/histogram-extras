{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Dist
  ( Gauss(..), sumW, sumWW, sumWX, sumWXY, nentries
  , Gauss0D, Gauss1D, Gauss2D
  , fillGauss
  ) where


import Control.Lens
import Data.Proxy
import Linear.Vector
import Linear.Matrix
import Data.Both
import Data.Serialize
import GHC.Generics


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



fillGauss :: (Additive v, Num a) => Gauss v a -> (v a, a) -> Gauss v a
fillGauss (Gauss sw sww swx swxy n) (v, w) =
  Gauss
    (sw + w)
    (sww + w * w)
    (w *^ v ^+^ swx)
    (outer v (w *^ v) !+! swxy)
    (n + 1)
