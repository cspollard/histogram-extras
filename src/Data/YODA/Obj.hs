{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.YODA.Obj ( Obj(..), _H1DD, _P1DD, YodaObj
                     , fillHist1D, yodaHist1D
                     , fillProf1D, yodaProf1D
                     , mergeYO, printYObj
                     , module X
                     ) where

import Control.Lens

import GHC.Generics
import Data.Serialize

import Data.Text (Text)
import qualified Data.Text as T

import Data.Semigroup ((<>))
import qualified Data.Map as M

import Data.Hist
import Data.Prof
import Data.Annotated as X
import Data.Weighted as X
import Data.Fillable as X

data Obj = H1DD !(Hist1D BinD Double)
         | P1DD !(Prof1D BinD Double)
         deriving (Show, Generic)

instance Serialize Obj where

makePrisms ''Obj

type YodaObj = Annotated Obj

mergeYO :: YodaObj -> YodaObj -> YodaObj
Annotated a (H1DD h) `mergeYO` Annotated _ (H1DD h') = Annotated a . H1DD $ addH h h'
Annotated a (P1DD p) `mergeYO` Annotated _ (P1DD p') = Annotated a . P1DD $ addP p p'
mergeYO _ _ = error "attempt to add two unrelated objects"

fillHist1D :: Double -> Double -> YodaObj -> YodaObj
fillHist1D w x = over (noted . _H1DD) (filling w x)

fillProf1D :: Double -> (Double, Double) -> YodaObj -> YodaObj
fillProf1D w xy = over (noted . _P1DD) (filling w xy)


-- TODO
-- path should be hard coded.

printYObj :: YodaObj -> Text
printYObj yo = T.unlines $
    case yo ^. noted of
        H1DD h ->
            [ "# BEGIN YODA_HISTO1D " <> pa
            , "Type=Histo1D"
            ] ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList $ yo ^. annots)
            ++ [ printHist1D h
            , "# END YODA_HISTO1D", ""
            ]

        P1DD p ->
            [ "# BEGIN YODA_PROFILE1D " <> pa
            , "Type=Profile1D"
            ] ++ fmap (\(k, v) -> k <> "=" <> v) (M.toList $ yo ^. annots)
            ++ [ printProf1D p
            , "# END YODA_PROFILE1D", ""
            ]

    where pa = yo ^. path



yodaHist1D :: Int -> Double -> Double -> YodaObj
yodaHist1D n mn mx = annotated . H1DD $ hist1D (binD mn n mx)

yodaProf1D :: Int -> Double -> Double -> YodaObj
yodaProf1D n mn mx = annotated . P1DD $ prof1D (binD mn n mx)


-- addYH :: YodaHist1D -> YodaHist1D -> YodaHist1D
-- addYH yh yh' = yh & noted %~ addH (view noted yh')

-- yodaProf1D :: Int -> Double -> Double -> YodaProf1D
-- yodaProf1D n mn mx = annotated . Prof1D $ histogram (binD mn n mx) (V.replicate n mempty)

-- addYP :: YodaProf1D -> YodaProf1D -> YodaProf1D
-- addYP yp yp' = yp & noted %~ addP (view noted yp')
