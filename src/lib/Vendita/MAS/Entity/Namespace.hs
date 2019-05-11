{-# LANGUAGE OverloadedStrings #-}

module Vendita.MAS.Entity.Namespace (
    NamespaceExtra(..),
    Namespace
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data NamespaceExtra = NamespaceExtra deriving (Show)

instance Extra NamespaceExtra where
    extraEntityClass = NAMESPACE 

instance FromJSON NamespaceExtra where
    parseJSON _ = return NamespaceExtra

type Namespace = Entity NamespaceExtra