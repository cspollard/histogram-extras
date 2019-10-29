{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.StrictMap
  ( StrictMap, strictMap, unSM, liftSM, liftSM2
  , singleton, inSM
  , lookup, mapMaybeWithKey
  ) where

import           Prelude hiding (zip, lookup)
import           Control.Lens
import           Data.Align
import           Data.These
import           Data.Data
import           Data.Functor.Classes
import qualified Data.Map.Strict  as M
import qualified Data.Map.Merge.Strict  as M
import           Data.Key hiding (zip)
import           Data.Serialize
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import Data.Functor.Bind


newtype StrictMap k a = SM { unSM :: M.Map k a }
  deriving (Show, Eq, Data, Typeable, Generic)


_StrictMap :: Iso (StrictMap k a) (StrictMap k b) (M.Map k a) (M.Map k b)
_StrictMap = coerced


strictMap :: M.Map k a -> StrictMap k a
strictMap = SM . force


force :: M.Map k a -> M.Map k a
force m = M.foldl' (flip seq) () m `seq` m


singleton :: k -> a -> StrictMap k a
singleton k = SM . M.singleton k


instance (Ord k, Serialize k, Serialize a)
  => Serialize (StrictMap k a) where
  put = put . M.toList . unSM
  get = strictMap . M.fromList <$> get


inSM :: (M.Map k a -> t) -> StrictMap k a -> t
inSM f (SM m) = f m


liftSM :: (M.Map k a -> M.Map k' a') -> StrictMap k a -> StrictMap k' a'
liftSM f = SM . f . unSM


liftSM2
  :: (M.Map k s -> M.Map k' s' -> M.Map k'' s'')
  -> StrictMap k s
  -> StrictMap k' s'
  -> StrictMap k'' s''
liftSM2 f (SM sm) (SM sm') = SM $ f sm sm'


type instance Key (StrictMap k) = k


instance Keyed (StrictMap k) where
  mapWithKey f (SM m) = SM $ M.mapWithKey f m


instance FoldableWithKey (StrictMap k) where
  foldMapWithKey f = foldMapWithKey f . unSM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictMap k a1 -> StrictMap k a
mapMaybeWithKey f (SM m) = SM $ M.mapMaybeWithKey f m


instance Functor (StrictMap k) where
  fmap f (SM m) = SM $ M.map f m

instance Foldable (StrictMap k) where
  foldr f start = foldr f start . unSM

instance Traversable (StrictMap k) where
  traverse f (SM m) = SM <$> M.traverseWithKey (const f) m


instance Ord k => Semialign (StrictMap k) where
  zipWith f = liftSM2 (M.intersectionWith f)

  alignWith f (SM m) (SM m') =
    SM
    $ M.merge
        (M.mapMissing (\_ x ->  f (This x)))
        (M.mapMissing (\_ y ->  f (That y)))
        (M.zipWithMatched (\_ x y -> f (These x y)))
        m
        m'


instance Ord k => Align (StrictMap k) where
  nil = mempty


instance Ord k => Apply (StrictMap k) where
  SM fs <.> SM xs = SM $ M.intersectionWith ($) fs xs


instance Ord k => Bind (StrictMap k) where
  SM m >>- f =
    SM $ M.mapMaybeWithKey (\k -> M.lookup k . view _StrictMap . f) m


instance Ord k => Trace (StrictMap k) where
  diagonal = join


instance Ord k => Semigroup (StrictMap k a) where
  SM m <> SM m' = SM $ M.union m m'


instance Ord k => Monoid (StrictMap k a) where
  mempty = SM M.empty
  mappend = (<>)


type instance Index (StrictMap k a) = k
type instance IxValue (StrictMap k a) = a


instance Ord k => FunctorWithIndex k (StrictMap k) where
  imap f = over _StrictMap (M.mapWithKey f)


instance Ord k => FoldableWithIndex k (StrictMap k) where
  ifoldMap f (SM m) = ifoldMap f m


instance Ord k => TraversableWithIndex k (StrictMap k) where
  itraverse f (SM m) = SM <$> itraverse f m


instance Ord k => At (StrictMap k a) where
  at k f (SM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SM m) (const (SM $ M.delete k m)) mv
      Just v' -> SM $ M.insert k v' m
    where mv = M.lookup k m


instance Ord k => Ixed (StrictMap k a) where
  ix k f (SM m) =
    case M.lookup k m of
     Just v  -> f v <&> \v' -> SM (M.insert k v' m)
     Nothing -> pure $ SM m


instance Ord k => IsList (StrictMap k a) where
  type Item (StrictMap k a) = (k, a)
  fromList = strictMap . fromList
  toList = toList . unSM


instance Show2 StrictMap where
  liftShowsPrec2 spk slk spv slv d (SM m) =
    liftShowsPrec2 spk slk spv slv d m


instance Show k => Show1 (StrictMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
