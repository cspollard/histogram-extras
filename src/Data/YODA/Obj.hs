{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Data.YODA.Obj
  ( YodaHist
  , Obj(..), _H1DD, _P1DD
  , YodaObj
  , YodaFolder
  , printProf, printHist
  , printYodaObj
  , mergeYO, mergeYF
  , module X
  ) where

import GHC.Generics
import           Data.Serialize
import           Control.Foldl
import           Control.Lens
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Map       as M
import           Data.Semigroup ((<>))
import           Data.Annotated as X
import qualified Data.Vector.Generic    as VG
import qualified Data.Vector.Generic.Mutable    as VGM

import           Data.Hist      as X

type HistFill m v a b c = FoldM m a (Histogram v b c)

-- TODO
-- these fns could all be monadic...
-- do I really want PrimMonads here?
-- is V.modify strict?
fill
  :: (Bin b, Monad m)
  => Histogram v b c
  -> (a -> (BinValue b, d))
  -> (c -> d -> c)
  -> HistFill m v a b c
fill h f comb = FoldM g (return h) return
  where
    g h' xs = do
      let (x, val) = f xs
      return $ over (atVal x) (flip f val)


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
  Just . Annotated a . H1DD $ addH h h'
Annotated a (P1DD p) `mergeYO` Annotated _ (P1DD p') =
  Just . Annotated a . P1DD $ addH p p'
mergeYO _ _ = Nothing

mergeYF :: YodaFolder -> YodaFolder -> YodaFolder
mergeYF yf yf' = M.mapMaybe id $ M.intersectionWith mergeYO yf yf'


printHist
  :: ( IntervalBin b, Vector v Text, Vector v (Dist1D a)
     , Vector v (BinValue b, BinValue b), Traversable v, Show (BinValue b)
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
  :: ( IntervalBin b, Vector v Text, Vector v (Dist2D a)
     , Vector v (BinValue b, BinValue b), Traversable v, Show (BinValue b)
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
