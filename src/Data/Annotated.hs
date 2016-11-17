{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Annotated ( Annotated(..), annotated
                      , annots, noted
                      , title, xlabel, ylabel
                      ) where


import Control.Lens
import GHC.Generics
import Data.Map.Strict
import Data.Text (Text)
import Data.Serialize.Text ()
import Data.Serialize

-- NB
-- this is strict in it's thing.
data Annotated a = Annotated { _annots :: Map Text Text
                             , _noted :: !a
                             } deriving (Generic, Show)

instance Serialize a => Serialize (Annotated a) where

makeLenses ''Annotated

annotated :: a -> Annotated a
annotated = Annotated empty

title :: Traversal' (Annotated a) Text
title = annots . ix "Title"

xlabel :: Traversal' (Annotated a) Text
xlabel = annots . ix "XLabel"

ylabel :: Traversal' (Annotated a) Text
ylabel = annots . ix "YLabel"
