{-# LANGUAGE OverloadedStrings #-}

module Vendita.MAS.Entity.Type (
    TypeExtra(..),
    Type
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data TypeExtra = TypeExtra deriving (Show)

instance Extra TypeExtra where
    extraEntityClass = TYPE 

instance FromJSON TypeExtra where
    parseJSON _ = return TypeExtra

type Type = Entity TypeExtra
