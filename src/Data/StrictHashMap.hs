{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.StrictHashMap
  ( StrictHashMap, strictHashMap, unSHM, liftSHM, liftSHM2
  , singleton, inSHM
  , lookup, mapMaybeWithKey
  ) where

import           Prelude hiding (zip, lookup)
import           Control.Lens
import           Data.Align
import           Data.Data
import           Data.Functor.Classes
import qualified Data.HashMap.Strict  as M
import           Data.Key hiding (zip)
import           Data.Serialize
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import Data.Functor.Bind
import Data.Hashable


newtype StrictHashMap k a = SHM { unSHM :: M.HashMap k a }
  deriving (Show, Eq, Data, Typeable, Generic)


_StrictHashMap :: Iso (StrictHashMap k a) (StrictHashMap k b) (M.HashMap k a) (M.HashMap k b)
_StrictHashMap = coerced


strictHashMap :: M.HashMap k a -> StrictHashMap k a
strictHashMap = SHM . force


force :: M.HashMap k a -> M.HashMap k a
force m = M.foldl' (flip seq) () m `seq` m


singleton :: Hashable k => k -> a -> StrictHashMap k a
singleton k = SHM . M.singleton k


instance (Hashable k, Eq k, Serialize k, Serialize a)
  => Serialize (StrictHashMap k a) where
  put = put . M.toList . unSHM
  get = strictHashMap . M.fromList <$> get


inSHM :: (M.HashMap k a -> t) -> StrictHashMap k a -> t
inSHM f (SHM m) = f m


liftSHM :: (M.HashMap k a -> M.HashMap k' a') -> StrictHashMap k a -> StrictHashMap k' a'
liftSHM f = SHM . f . unSHM


liftSHM2
  :: (M.HashMap k s -> M.HashMap k' s' -> M.HashMap k'' s'')
  -> StrictHashMap k s
  -> StrictHashMap k' s'
  -> StrictHashMap k'' s''
liftSHM2 f (SHM sm) (SHM sm') = SHM $ f sm sm'


type instance Key (StrictHashMap k) = k


instance Keyed (StrictHashMap k) where
  mapWithKey f (SHM m) = SHM $ M.mapWithKey f m


instance FoldableWithKey (StrictHashMap k) where
  foldMapWithKey f = foldMapWithKey f . unSHM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictHashMap k a1 -> StrictHashMap k a
mapMaybeWithKey f (SHM m) = SHM $ M.mapMaybeWithKey f m


instance Functor (StrictHashMap k) where
  fmap f (SHM m) = SHM $ M.map f m
  {-# INLINE fmap  #-}

instance Foldable (StrictHashMap k) where
  foldr f start = foldr f start . unSHM

instance Traversable (StrictHashMap k) where
  traverse f (SHM m) = SHM <$> M.traverseWithKey (const f) m


instance (Hashable k, Eq k) => Semialign (StrictHashMap k) where
  zipWith f = liftSHM2 (M.intersectionWith f)

  alignWith f (SHM m) (SHM m') = SHM $ alignWith f m m'


instance (Hashable k, Eq k) => Align (StrictHashMap k) where
  nil = mempty


instance (Hashable k, Eq k) => Apply (StrictHashMap k) where
  SHM fs <.> SHM xs = SHM $ M.intersectionWith ($) fs xs


instance (Hashable k, Eq k) => Bind (StrictHashMap k) where
  SHM m >>- f =
    SHM $ M.mapMaybeWithKey (\k -> M.lookup k . view _StrictHashMap . f) m


instance (Hashable k, Eq k) => Trace (StrictHashMap k) where
  diagonal = join


instance (Hashable k, Eq k) => Semigroup (StrictHashMap k a) where
  SHM m <> SHM m' = SHM $ M.union m m'


instance (Hashable k, Eq k) => Monoid (StrictHashMap k a) where
  mempty = SHM M.empty
  mappend = (<>)


type instance Index (StrictHashMap k a) = k
type instance IxValue (StrictHashMap k a) = a


instance (Hashable k, Eq k) => FunctorWithIndex k (StrictHashMap k) where
  imap f = over _StrictHashMap (M.mapWithKey f)


instance (Hashable k, Eq k) => FoldableWithIndex k (StrictHashMap k) where
  ifoldMap f (SHM m) = ifoldMap f m


instance (Hashable k, Eq k) => TraversableWithIndex k (StrictHashMap k) where
  itraverse f (SHM m) = SHM <$> itraverse f m


instance (Hashable k, Eq k) => At (StrictHashMap k a) where
  at k f (SHM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SHM m) (const (SHM $ M.delete k m)) mv
      Just v' -> SHM $ M.insert k v' m
    where mv = M.lookup k m


instance (Hashable k, Eq k) => Ixed (StrictHashMap k a) where
  ix k f (SHM m) =
    case M.lookup k m of
     Just v  -> f v <&> \v' -> SHM (M.insert k v' m)
     Nothing -> pure $ SHM m


instance (Hashable k, Eq k) => IsList (StrictHashMap k a) where
  type Item (StrictHashMap k a) = (k, a)
  fromList = strictHashMap . fromList
  toList = toList . unSHM


instance Show2 StrictHashMap where
  liftShowsPrec2 spk slk spv slv d (SHM m) =
    liftShowsPrec2 spk slk spv slv d m


instance Show k => Show1 (StrictHashMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
