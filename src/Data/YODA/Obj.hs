{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.YODA.Obj
  ( Obj(..), _H1DD, _P1DD, _H2DD
  , YodaObj
  , YodaFolder
  , printYodaObj
  , mergeYO, mergeYF, prefixF
  , oneDObj, twoDObj
  , Folder(..), singleton, inF, inF2
  , module X
  ) where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Semigroup
import           Data.Serialize
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics

import           Data.Annotated  as X
import           Data.Hist       as X


data Obj =
  H1DD !(Hist1D (ArbBin Double))
  | P1DD !(Prof1D (ArbBin Double))
  | H2DD !(Hist2D (ArbBin Double) (ArbBin Double))
  deriving Generic

makePrisms ''Obj

-- TODO
-- these should perhaps throw errors...
instance Semigroup Obj where
  x@(H1DD h) <> (H1DD h') = maybe x H1DD $ hadd' h h'
  x@(P1DD h) <> (P1DD h') = maybe x P1DD $ hadd' h h'
  x@(H2DD h) <> (H2DD h') = maybe x H2DD $ hadd' h h'
  x <> _ = x

instance Serialize Obj where

type YodaObj = Annotated Obj

type YodaFolder = M.Map Text YodaObj

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


mergeYF :: YodaFolder -> YodaFolder -> YodaFolder
mergeYF yf yf' = M.mapMaybe id $ M.intersectionWith mergeYO yf yf'


printYodaObj :: Text -> YodaObj -> Text
printYodaObj pa (Annotated as (H1DD h)) =
  T.unlines $
    [ "# BEGIN YODA_HISTO1D " <> pa
    , "Type=Histo1D"
    , "Path=" <> pa
    ]
    ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList as)
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
    ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList as)
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
    ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList as)
    ++
      [ printHistogram printDist2D printBin1D p
      , "# END YODA_PROFILE1D", ""
      ]

prefixF :: Text -> Folder a -> Folder a
prefixF p = inF $ M.mapKeysMonotonic (p <>)


newtype Folder a = Folder { folderToMap :: M.Map T.Text a }
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance Serialize a => Serialize (Folder a) where

inF :: (M.Map T.Text a -> M.Map T.Text b) -> Folder a -> Folder b
inF f = Folder . f . folderToMap

inF2 :: (M.Map T.Text t1 -> M.Map T.Text t -> M.Map T.Text a) -> Folder t1 -> Folder t -> Folder a
inF2 f (Folder m) (Folder m') = Folder $ f m m'

singleton :: T.Text -> a -> Folder a
singleton n = Folder . M.singleton n

instance Semigroup a => Monoid (Folder a) where
  mempty = Folder M.empty
  mappend = inF2 (M.unionWith (<>))
