{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Prototype (
    PrototypeExtra(..),
    PrototypeField(..),
    Prototype,
    getPrototype,
    getPrototypeVersion,
    listPrototypes
)

where

import Data.Aeson
import Data.Set (Set)
import Network.HTTP.Req ((=:))
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Meta
import Vendita.MAS.Resource

data PrototypeField = PrototypeField {
    prototypeFieldName :: String,
    prototypeFieldDescription :: String,
    prototypeFieldType :: String,
    prototypeFieldIsRequired :: Bool,
    prototypeFieldDefault :: Maybe String,
    prototypeFieldMeta :: Meta,
    prototypeFieldPlaceholder :: Maybe String,
    prototypeFieldPosition :: Int
} deriving (Show)

instance Eq PrototypeField where
    a == b = (prototypeFieldName a) == (prototypeFieldName b)

instance Ord PrototypeField where
    compare a b = compare (prototypeFieldName a) (prototypeFieldName b)

instance FromJSON PrototypeField where
    parseJSON = withObject "field" $ \o -> do
        prototypeFieldName <- o .: "name"
        prototypeFieldDescription <- o .: "description"
        prototypeFieldType <- o .: "data_type"
        prototypeFieldIsRequired <- o .: "is_required"
        prototypeFieldDefault <- o .: "deflt"
        prototypeFieldMeta <- o .: "meta"
        prototypeFieldPlaceholder <- o .: "placeholder"
        prototypeFieldPosition <- o .: "position"
        return PrototypeField{..}

data PrototypeExtra = PrototypeExtra {
    prototypeVersion :: Int,
    prototypeIsCurrent :: Bool,
    prototypeTitle :: String,
    prototypeFields :: Set PrototypeField
} deriving (Show)

instance Extra PrototypeExtra where
    extraEntityClass = PROTOTYPE 

instance FromJSON PrototypeExtra where
    parseJSON = withObject "prototype" $ \o -> do
        prototypeVersion <- o .: "version"
        prototypeIsCurrent <- o .: "is_current"
        prototypeTitle <- o .: "title"
        prototypeFields <- o .: "fields"
        return PrototypeExtra{..}

type Prototype = Entity PrototypeExtra

getPrototype = getResource @Prototype
listPrototypes = listResource @Prototype

getPrototypeVersion :: Identifier Prototype -> Int -> MAS Prototype
getPrototypeVersion identifier version = fmap envelopeFirst $ withResource @Prototype $ withIdentifier identifier $ withOption ("version" =: version) $ get
