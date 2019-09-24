{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Annotated
  ( Annotated, annotated
  , pattern Annotated
  , title, xlabel, ylabel, zlabel
  ) where


import Control.Lens (at)
import Data.Map.Strict
import Data.Both
import Data.Profunctor.Optic
import Data.Text           (Text)


-- TODO
-- these maps are not really strict.
type Annotated = Both (Map Text Text)


pattern Annotated :: Map Text Text -> a -> Annotated a
pattern Annotated x y = Both x y


annotated :: a -> Annotated a
annotated = pure


title :: Traversal' (Annotated a) (Maybe Text)
title = _1 . wander (at "Title")


xlabel :: Traversal' (Annotated a) (Maybe Text)
xlabel = _1 . wander (at "XLabel")


ylabel :: Traversal' (Annotated a) (Maybe Text)
ylabel = _1 . wander (at "YLabel")


zlabel :: Traversal' (Annotated a) (Maybe Text)
zlabel = _1 . wander (at "ZLabel")
