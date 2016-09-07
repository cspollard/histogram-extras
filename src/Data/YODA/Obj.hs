{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.YODA.Obj where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T

import Data.Semigroup ((<>))

import Data.Hist
import Data.Prof
import Data.Annotated

data Obj = H1DD (Hist1D BinD Double)
         | P1DD (Prof1D BinD Double)

makePrisms ''Obj

type YodaObj = Annotated Obj

fillHist1D :: (Double, Double) -> YodaObj -> YodaObj
fillHist1D wx = over (noted . _H1DD) (fill wx)


-- TODO
-- path should be hard coded.

printYP1DD :: YodaObj -> Text
printYP1DD yo = T.unlines $ let p = yo ^. path
                                xl = yo ^. xlabel
                                yl = yo ^. ylabel
                            in  [ "# BEGIN YODA_PROFILE1D " <> p, "Path=" <> p, "Type=Profile1D"
                                , "XLabel=" <> xl, "YLabel=" <> yl
                                , views (noted . _P1DD) printProf1D yo
                                , "# END YODA_PROFILE1D", ""
                                ]


printYH1DD :: YodaObj -> Text
printYH1DD yo = T.unlines $ let p = yo ^. path
                                xl = yo ^. xlabel
                                yl = yo ^. ylabel
                            in  [ "# BEGIN YODA_HISTO1D " <> p, "Path=" <> p, "Type=Histo1D"
                                , "XLabel=" <> xl, "YLabel=" <> yl
                                , views (noted . _H1DD) printHist1D yo
                                , "# END YODA_HISTO1D", ""
                                ]



-- yodaHist1D :: Int -> Double -> Double -> YodaHist1D
-- yodaHist1D n mn mx = annotated . Hist1D $ histogram (binD mn n mx) (V.replicate n mempty)


-- addYH :: YodaHist1D -> YodaHist1D -> YodaHist1D
-- addYH yh yh' = yh & noted %~ addH (view noted yh')

-- yodaProf1D :: Int -> Double -> Double -> YodaProf1D
-- yodaProf1D n mn mx = annotated . Prof1D $ histogram (binD mn n mx) (V.replicate n mempty)

-- addYP :: YodaProf1D -> YodaProf1D -> YodaProf1D
-- addYP yp yp' = yp & noted %~ addP (view noted yp')
