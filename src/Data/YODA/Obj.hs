{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.YODA.Obj
  ( HistFill
  , Obj(..), _H1DD, _P1DD
  , YodaObj
  , YodaFolder
  , fill
  , printProf, printHist
  , printYodaObj
  , mergeYO, mergeYF
  , module X
  ) where

import qualified Control.Foldl       as F
import           Control.Lens
import           Data.Annotated      as X
import qualified Data.Map            as M
import           Data.Semigroup      ((<>))
import           Data.Serialize
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           GHC.Generics

import           Data.Hist           as X

type HistFill v b a c = F.Fold a (Histogram v b c)

-- convert everything to Unbox vectors in order to make sure updating is strict
fill
  :: forall v a b c d. (VG.Vector v c, VU.Unbox c, Bin b)
  => Histogram v b c
  -> (a -> (BinValue b, d))
  -> (c -> d -> c)
  -> HistFill v b a c
fill h f comb = F.Fold g h' (over histData VG.convert)
  where
    h' :: Histogram VU.Vector b c
    h' = over histData VG.convert h
    g h'' xs =
      let (x, val) = f xs
      in over (atVal x) (`comb` val) h''



data Obj =
  H1DD !(Histogram V.Vector (ArbBin Double) (Dist1D Double))
  | P1DD !(Histogram V.Vector (ArbBin Double) (Dist2D Double))
  deriving (Generic, Show)

makePrisms ''Obj

instance Serialize Obj where

type YodaObj = Annotated Obj

type YodaFolder = M.Map Text YodaObj

mergeYO :: YodaObj -> YodaObj -> Maybe YodaObj
Annotated a (H1DD h) `mergeYO` Annotated _ (H1DD h') =
  Annotated a . H1DD <$> hadd h h'
Annotated a (P1DD p) `mergeYO` Annotated _ (P1DD p') =
  Annotated a . P1DD <$> hadd p p'
mergeYO _ _ = Nothing

mergeYF :: YodaFolder -> YodaFolder -> YodaFolder
mergeYF yf yf' = M.mapMaybe id $ M.intersectionWith mergeYO yf yf'


printHist
  :: ( IntervalBin b, VG.Vector v Text, VG.Vector v (Dist1D a)
     , VG.Vector v (BinValue b, BinValue b), Traversable v, Show (BinValue b)
     , Show a, Num a )
  => Text -> Annotated (Histogram v b (Dist1D a)) -> Text
printHist pa (Annotated as h) =
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

printProf
  :: ( IntervalBin b, VG.Vector v Text, VG.Vector v (Dist2D a)
     , VG.Vector v (BinValue b, BinValue b), Traversable v, Show (BinValue b)
     , Show a, Num a )
  => Text -> Annotated (Histogram v b (Dist2D a)) -> Text
printProf pa (Annotated as p) =
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

printYodaObj :: Text -> YodaObj -> Text
printYodaObj pa (Annotated a (H1DD h)) = printHist pa (Annotated a h)
printYodaObj pa (Annotated a (P1DD p)) = printProf pa (Annotated a p)
