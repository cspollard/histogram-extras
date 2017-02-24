{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Histogram.Instances () where

import           Data.Serialize
import           Data.Vector.Fixed            as V
import           Data.Vector.Unboxed
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics

deriving instance Generic (Empty a)
instance Serialize a => Serialize (Empty a) where

deriving instance Generic (Only a)
instance Serialize a => Serialize (Only a) where

derivingUnbox "Empty"
  [t| forall a. Empty a -> () |]
  [| const () |]
  [| const Empty |]

derivingUnbox "Only"
  [t| forall a. Unbox a => Only a -> a |]
  [| V.head |]
  [| Only |]
