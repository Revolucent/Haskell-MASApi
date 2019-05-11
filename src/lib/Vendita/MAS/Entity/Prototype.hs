{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Prototype (
    PrototypeExtra(..),
    Prototype
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data PrototypeExtra = PrototypeExtra {
    prototypeVersion :: Int,
    prototypeIsCurrent :: Bool,
    prototypeTitle :: String
} deriving (Show)

instance Extra PrototypeExtra where
    extraEntityClass = PROTOTYPE 

instance FromJSON PrototypeExtra where
    parseJSON = withObject "prototype" $ \o -> do
        prototypeVersion <- o .: "version"
        prototypeIsCurrent <- o .: "is_current"
        prototypeTitle <- o .: "title"
        return PrototypeExtra{..}

type Prototype = Entity PrototypeExtra
