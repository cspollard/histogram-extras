{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MultiFill where

import Control.Lens

import qualified Data.Map.Strict as M

import Data.Fillable

newtype MultiFill a b = MultiFill { _fromMap :: M.Map a b }

makeLenses ''MultiFill

instance (Ord a, Fillable b) => Fillable (MultiFill a b) where
    type FillVec (MultiFill a b) = M.Map a (FillVec b)
    fill ws = over fromMap $
            M.mergeWithKey
                (\_ a -> Just . fill a)
                (error "attempting to fill key that doesn't exist.")
                id
            ws
