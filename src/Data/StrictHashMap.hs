{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.StrictHashMap
  ( StrictHashMap, strictHashMap
  , intersectionWith, mapMaybeWithKey
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Align
import           Data.Data
import           Data.Functor.Classes
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.Key
import           Data.Semigroup
import           Data.Serialize
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import           Prelude              hiding (lookup)


newtype StrictHashMap k a = SHM (HM.HashMap k a)
  deriving (Show, Eq, Data, Typeable, Generic)

makePrisms ''StrictHashMap

instance (NFData k, NFData a) => NFData (StrictHashMap k a) where

strictHashMap :: HM.HashMap k a -> StrictHashMap k a
strictHashMap = SHM . force'

force' :: HM.HashMap k a -> HM.HashMap k a
force' m = HM.foldl' (flip seq) () m `seq` m

instance (Ord k, Hashable k, Serialize k, Serialize a)
  => Serialize (StrictHashMap k a) where
  put = put . HM.toList . view _SHM
  get = SHM . HM.fromList <$> get

type instance Key (StrictHashMap k) = k

instance Keyed (StrictHashMap k) where
  mapWithKey f (SHM m) = SHM $ HM.mapWithKey f m

instance FoldableWithKey (StrictHashMap k) where
  foldMapWithKey f = foldMapWithKey f . view _SHM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictHashMap k a1 -> StrictHashMap k a
mapMaybeWithKey f (SHM m) = SHM $ HM.mapMaybeWithKey f m

intersectionWith
  :: (Hashable k, Ord k)
  => (a1 -> b -> a) -> StrictHashMap k a1 -> StrictHashMap k b -> StrictHashMap k a
intersectionWith f (SHM m) (SHM m') = SHM $ HM.intersectionWith f m m'

instance Functor (StrictHashMap k) where
  fmap f (SHM m) = SHM $ HM.map f m

instance Foldable (StrictHashMap k) where
  foldr f start = foldr f start . view _SHM

instance Traversable (StrictHashMap k) where
  traverse f (SHM m) = SHM <$> HM.traverseWithKey (const f) m

instance (Hashable k, Ord k) => Align (StrictHashMap k) where
  nil = mempty
  align (SHM m) (SHM m') = strictHashMap $ align m m'

instance (Hashable k, Ord k) => Trace (StrictHashMap k) where
  diagonal (SHM m) = strictHashMap . diagonal $ view _SHM <$> m

instance (Hashable k, Ord k) => Semigroup (StrictHashMap k a) where
  SHM m <> SHM m' = SHM $ HM.union m m'

instance (Hashable k, Ord k) => Monoid (StrictHashMap k a) where
  mempty = SHM HM.empty
  mappend = (<>)

type instance Index (StrictHashMap k a) = k
type instance IxValue (StrictHashMap k a) = a


instance (Hashable k, Ord k) => At (StrictHashMap k a) where
  at k f (SHM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SHM m) (const (SHM $ HM.delete k m)) mv
      Just v' -> SHM $ HM.insert k v' m
    where mv = HM.lookup k m


instance (Hashable k, Ord k) => Ixed (StrictHashMap k a) where
  ix k f (SHM m) =
    case HM.lookup k m of
     Just v  -> f v <&> \v' -> SHM (HM.insert k v' m)
     Nothing -> pure $ SHM m


instance (Hashable k, Ord k) => IsList (StrictHashMap k a) where
  type Item (StrictHashMap k a) = (k, a)
  fromList = strictHashMap . fromList
  toList = toList . view _SHM


instance Show2 StrictHashMap where
  liftShowsPrec2 spk slk spv slv d (SHM m) =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (HM.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (StrictHashMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
