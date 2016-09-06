{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.YODA.Annotated ( Annotated, annots, thing
                           , path, xlabel, ylabel
                           ) where


import Control.Lens
import GHC.Generics
import Data.HashMap.Strict
import Data.Text (Text)

-- NB
-- this is strict in it's thing.
data Annotated a = Annotated { _annots :: HashMap Text Text
                             , _thing :: !a
                             } deriving (Generic, Show)

makeLenses ''Annotated

path :: Traversal' (Annotated a) Text
path = annots . ix "Path"

xlabel :: Traversal' (Annotated a) Text
xlabel = annots . ix "XLabel"

ylabel :: Traversal' (Annotated a) Text
ylabel = annots . ix "YLabel"
