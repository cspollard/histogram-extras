{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.StrictIntMap
  ( StrictIntMap, strictIntMap, unSIM, liftSIM, liftSIM2
  , singleton, inSIM
  , lookup, mapMaybeWithKey
  ) where

import           Prelude hiding (zip, lookup)
import           Control.Lens
import           Data.Align
import           Data.These
import           Data.Data
import           Data.Functor.Classes
import qualified Data.IntMap.Strict  as M
import qualified Data.IntMap.Merge.Strict  as M
import           Data.Key hiding (zip)
import           Data.Serialize
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import Data.Functor.Bind


newtype StrictIntMap a = SIM { unSIM :: M.IntMap a }
  deriving (Show, Eq, Data, Typeable, Generic)


_StrictIntMap :: Iso (StrictIntMap a) (StrictIntMap b) (M.IntMap a) (M.IntMap b)
_StrictIntMap = coerced


strictIntMap :: M.IntMap a -> StrictIntMap a
strictIntMap = SIM . force


force :: M.IntMap a -> M.IntMap a
force m = M.foldl' (flip seq) () m `seq` m


singleton :: Int -> a -> StrictIntMap a
singleton k = SIM . M.singleton k


instance Serialize a => Serialize (StrictIntMap a) where
  put = put . M.toList . unSIM
  get = strictIntMap . M.fromList <$> get


inSIM :: (M.IntMap a -> t) -> StrictIntMap a -> t
inSIM f (SIM m) = f m


liftSIM :: (M.IntMap a -> M.IntMap a') -> StrictIntMap a -> StrictIntMap a'
liftSIM f = SIM . f . unSIM


liftSIM2
  :: (M.IntMap s -> M.IntMap s' -> M.IntMap s'')
  -> StrictIntMap s
  -> StrictIntMap s'
  -> StrictIntMap s''
liftSIM2 f (SIM sm) (SIM sm') = SIM $ f sm sm'


type instance Key (StrictIntMap) = Int


instance Keyed (StrictIntMap) where
  mapWithKey f (SIM m) = SIM $ M.mapWithKey f m


instance FoldableWithKey (StrictIntMap) where
  foldMapWithKey f = foldMapWithKey f . unSIM


mapMaybeWithKey :: (Int -> a -> Maybe b) -> StrictIntMap a -> StrictIntMap b
mapMaybeWithKey f (SIM m) = SIM $ M.mapMaybeWithKey f m


instance Functor (StrictIntMap) where
  fmap f (SIM m) = SIM $ M.map f m

instance Foldable (StrictIntMap) where
  foldr f start = foldr f start . unSIM

instance Traversable (StrictIntMap) where
  traverse f (SIM m) = SIM <$> M.traverseWithKey (const f) m


instance Semialign (StrictIntMap) where
  zipWith f = liftSIM2 (M.intersectionWith f)

  alignWith f (SIM m) (SIM m') =
    SIM
    $ M.merge
        (M.mapMissing (\_ x ->  f (This x)))
        (M.mapMissing (\_ y ->  f (That y)))
        (M.zipWithMatched (\_ x y -> f (These x y)))
        m
        m'


instance Align (StrictIntMap) where
  nil = mempty


instance Apply (StrictIntMap) where
  SIM fs <.> SIM xs = SIM $ M.intersectionWith ($) fs xs


instance Bind (StrictIntMap) where
  SIM m >>- f =
    SIM $ M.mapMaybeWithKey (\k -> M.lookup k . view _StrictIntMap . f) m


instance Trace (StrictIntMap) where
  diagonal = join


instance Semigroup (StrictIntMap a) where
  SIM m <> SIM m' = SIM $ M.union m m'


instance Monoid (StrictIntMap a) where
  mempty = SIM M.empty
  mappend = (<>)


type instance Index (StrictIntMap a) = Int
type instance IxValue (StrictIntMap a) = a


instance FunctorWithIndex Int StrictIntMap where
  imap f = over _StrictIntMap (M.mapWithKey f)


instance FoldableWithIndex Int StrictIntMap where
  ifoldMap f (SIM m) = ifoldMap f m


instance TraversableWithIndex Int StrictIntMap where
  itraverse f (SIM m) = SIM <$> itraverse f m


instance At (StrictIntMap a) where
  at k f (SIM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SIM m) (const (SIM $ M.delete k m)) mv
      Just v' -> SIM $ M.insert k v' m
    where mv = M.lookup k m


instance Ixed (StrictIntMap a) where
  ix k f (SIM m) =
    case M.lookup k m of
     Just v  -> f v <&> \v' -> SIM (M.insert k v' m)
     Nothing -> pure $ SIM m


instance IsList (StrictIntMap a) where
  type Item (StrictIntMap a) = (Int, a)
  fromList = strictIntMap . fromList
  toList = toList . unSIM


instance Show1 StrictIntMap where
  liftShowsPrec sp sl d (SIM m) =
    liftShowsPrec sp sl d m
