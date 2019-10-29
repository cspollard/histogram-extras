{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Annotated
  ( Annotated(..), annotated
  , _Annotated, Notes
  , notes, noted
  , title, xlabel, ylabel, zlabel
  ) where


import Control.Lens
import Data.StrictMap
import Both
import Data.Functor.Bind
import Data.Text           (Text)

 
type Notes = StrictMap Text Text

newtype Annotated a = Annotated (Both Notes a)
  deriving (Functor, Apply, Applicative, Bind, Monad) via (Both (StrictMap Text Text))


annotated :: Notes -> a -> Annotated a
annotated n a = Annotated (Both n a)


_Annotated :: Iso (Annotated a) (Annotated b) (Both (StrictMap Text Text) a) (Both (StrictMap Text Text) b)
_Annotated = coerced


notes :: Lens' (Annotated a) (StrictMap Text Text)
notes = _Annotated . _1


noted :: Lens (Annotated a) (Annotated b) a b
noted = _Annotated . _2


title :: Lens' (Annotated a) (Maybe Text)
title = notes . (at "Title")


xlabel :: Lens' (Annotated a) (Maybe Text)
xlabel = notes . (at "XLabel")


ylabel :: Lens' (Annotated a) (Maybe Text)
ylabel = notes . (at "YLabel")


zlabel :: Lens' (Annotated a) (Maybe Text)
zlabel = notes . (at "ZLabel")
