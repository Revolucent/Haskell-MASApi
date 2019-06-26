{-# LANGUAGE OverloadedStrings #-}

module Vendita.MAS.Entity.Generic (
    NothingExtra(..),
    Generic
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data NothingExtra = NothingExtra deriving (Show)

instance Extra NothingExtra where
    extraEntityClass = NOTHING 

instance FromJSON NothingExtra where
    parseJSON _ = return NothingExtra

type Generic = Entity NothingExtra
