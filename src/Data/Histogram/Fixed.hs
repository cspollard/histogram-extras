
module Data.Histogram.Fixed
  ( module X
  , HistogramN
  ) where


import           Data.Histogram.Bin.Fixed as X
import           Data.Histogram.Generic   as X
import           Linear.V

type HistogramN n b a = Histogram (V n) (b n) a
