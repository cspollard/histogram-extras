{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.YODA.Obj
  ( Obj(..), _H1DD, _P1DD, _H2DD
  , YodaObj
  , YodaFolder
  , printYodaObj
  , mergeYO
  , oneDObj, twoDObj
  , module X
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Trans.Writer.Strict
import           Data.Align
import           Data.Annotated                    as X
import           Data.Hist                         as X
import           Data.Histogram.Bin.Arb            as X
import           Data.Semigroup
import           Data.Serialize
import           Data.StrictHashMap
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Tuple                        (swap)
import           GHC.Exts
import           GHC.Generics


data Obj =
  H1DD !(Hist1D (ArbBin Double))
  | P1DD !(Prof1D (ArbBin Double))
  | H2DD !(Hist2D (ArbBin Double) (ArbBin Double))
  deriving Generic

makePrisms ''Obj

instance NFData Obj where

-- TODO
-- these should perhaps throw errors...
instance Semigroup Obj where
  x@(H1DD h) <> (H1DD h') = maybe x H1DD $ hadd' h h'
  x@(P1DD h) <> (P1DD h') = maybe x P1DD $ hadd' h h'
  x@(H2DD h) <> (H2DD h') = maybe x H2DD $ hadd' h h'
  x <> _ = x

instance Serialize Obj where

type YodaObj = Annotated Obj

type YodaFolder = Folder YodaObj

oneDObj :: YodaObj -> Bool
oneDObj (Annotated _ (H1DD _)) = True
oneDObj (Annotated _ (H2DD _)) = False
oneDObj (Annotated _ (P1DD _)) = True

twoDObj :: YodaObj -> Bool
twoDObj (Annotated _ (H1DD _)) = False
twoDObj (Annotated _ (H2DD _)) = True
twoDObj (Annotated _ (P1DD _)) = False

mergeYO :: YodaObj -> YodaObj -> Maybe YodaObj
Annotated a (H1DD h) `mergeYO` Annotated _ (H1DD h') =
  Annotated a . H1DD <$> hadd' h h'
Annotated a (H2DD h) `mergeYO` Annotated _ (H2DD h') =
  Annotated a . H2DD <$> hadd' h h'
Annotated a (P1DD p) `mergeYO` Annotated _ (P1DD p') =
  Annotated a . P1DD <$> hadd' p p'
mergeYO _ _ = Nothing

printYodaObj :: Text -> YodaObj -> Text
printYodaObj pa (Annotated as (H1DD h)) =
  T.unlines $
    [ "# BEGIN YODA_HISTO1D " <> pa
    , "Type=Histo1D"
    , "Path=" <> pa
    ]
    ++ fmap (\(k, v) -> k <> "=" <> v) (toList as)
    ++
      [ printHistogram printDist1D printBin1D h
      , "# END YODA_HISTO1D", ""
      ]

printYodaObj pa (Annotated as (H2DD h)) =
  T.unlines $
    [ "# BEGIN YODA_HISTO2D " <> pa
    , "Type=Histo2D"
    , "Path=" <> pa
    ]
    ++ fmap (\(k, v) -> k <> "=" <> v) (toList as)
    ++
      [ -- we can't yet handle overflows in YODA
        printHistogram printDist2D printBin2D . set outOfRange Nothing $ h
      , "# END YODA_HISTO2D", ""
      ]

printYodaObj pa (Annotated as (P1DD p)) =
  T.unlines $
    [ "# BEGIN YODA_PROFILE1D " <> pa
    , "Type=Profile1D"
    , "Path=" <> pa
    ]
    ++ fmap (\(k, v) -> k <> "=" <> v) (toList as)
    ++
      [ printHistogram printDist2D printBin1D p
      , "# END YODA_PROFILE1D", ""
      ]

newtype Folder a = Folder (Free (StrictHashMap T.Text) a)
  deriving (Functor, Applicative, Monad, Traversable, Foldable)

instance Monoid a => Monoid (Folder a) where
  mempty = Folder $ Free mempty
  m `mappend` m' =
    let (Folder (Free x)) = flatten m
        (Folder (Free x')) = flatten m'
    in Folder . Free $ alignWith mergeThese x x'




flatten :: Folder a -> Folder a
flatten (Folder f) = Folder . Free . fromList . fmap g . runWriterT $ foldFree go f
  where
    g (x, t) = (t, Pure x)
    go :: StrictHashMap T.Text a -> WriterT T.Text [] a
    go m = WriterT . fmap swap $ toList m


-- instance NFData a => NFData (Folder a) where
-- instance Serialize a => Serialize (Folder a) where
--
-- type instance Index (Folder a) = T.Text
-- type instance IxValue (Folder a) = a
--
-- instance Ixed (Folder a) where
--   ix i = toMap . ix i
--
-- instance At (Folder a) where
--   at i = toMap . at i
--
-- prefixF :: Text -> Folder a -> Folder a
-- prefixF =
--
-- instance Semigroup a => Monoid (Folder a) where
--   mempty = Folder mempty
--   mappend = inF2 (M.unionWith (<>))
