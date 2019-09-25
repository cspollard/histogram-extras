{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Annotated
  ( Annotated, annotated
  , pattern Annotated
  , title, xlabel, ylabel, zlabel
  ) where


import Control.Lens hiding (_1)
import Data.Map.Strict
import Data.Both
import Data.Text           (Text)
import Data.Profunctor.Optic (hubble, starry)


-- TODO
-- these maps are not really strict.
type Annotated = Both (Map Text Text)


pattern Annotated :: Map Text Text -> a -> Annotated a
pattern Annotated x y = Both x y


annotated :: a -> Annotated a
annotated = pure


title :: Lens' (Annotated a) (Maybe Text)
title = hubble _1 . (at "Title")


xlabel :: Lens' (Annotated a) (Maybe Text)
xlabel = hubble _1 . (at "XLabel")


ylabel :: Lens' (Annotated a) (Maybe Text)
ylabel = hubble _1 . (at "YLabel")


zlabel :: Lens' (Annotated a) (Maybe Text)
zlabel = hubble _1 . (at "ZLabel")
