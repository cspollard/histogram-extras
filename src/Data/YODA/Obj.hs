{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.YODA.Obj
  ( Obj(..), _H1DD, _P1DD
  , YodaObj
  , YodaFolder
  , printYodaObj
  , mergeYO, mergeYF
  , module X
  ) where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Semigroup  ((<>))
import           Data.Serialize
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics

import           Data.Annotated  as X
import           Data.Hist       as X


data Obj =
  H1DD !(Hist1D (ArbBin Double))
  | P1DD !(Prof1D (ArbBin Double))
  deriving (Generic, Show)

makePrisms ''Obj

instance Serialize Obj where

type YodaObj = Annotated Obj

type YodaFolder = M.Map Text YodaObj

mergeYO :: YodaObj -> YodaObj -> Maybe YodaObj
Annotated a (H1DD h) `mergeYO` Annotated _ (H1DD h') =
  Annotated a . H1DD <$> hadd' h h'
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
      [ printHistogram printDist1D h
      , "# END YODA_HISTO1D", ""
      ]

printYodaObj pa (Annotated as (P1DD p)) =
  T.unlines $
    [ "# BEGIN YODA_PROFILE1D " <> pa
    , "Type=Profile1D"
    , "Path=" <> pa
    ]
    ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList as)
    ++
      [ printHistogram printDist2D p
      , "# END YODA_PROFILE1D", ""
      ]
