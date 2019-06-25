{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Prototype (
    Prototype(..),
    PrototypeExtra(..),
    PrototypeField(..),
    PrototypeGroup(..),
    createPrototype,
    createPrototypeField,
    deletePrototype,
    getPrototype,
    getPrototypeVersion,
    listPrototypes,
    prototypeFieldMetaEditor
)

where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
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
    prototypeFieldIsRepeatable :: Bool,
    prototypeFieldDefault :: Maybe String,
    prototypeFieldMeta :: Meta,
    prototypeFieldPlaceholder :: Maybe String,
    prototypeFieldPosition :: Int
} deriving (Show)

prototypeFieldMetaEditor :: PrototypeField -> Maybe String
prototypeFieldMetaEditor = metaEditor . prototypeFieldMeta

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
        prototypeFieldIsRepeatable <- o .: "is_repeatable"
        prototypeFieldDefault <- o .: "deflt"
        prototypeFieldMeta <- o .: "meta"
        prototypeFieldPlaceholder <- o .: "placeholder"
        prototypeFieldPosition <- o .: "position"
        return PrototypeField{..}

instance ToJSON PrototypeField where
    toJSON PrototypeField{..} = object $ filterNulls [
            "name" .= prototypeFieldName,
            "description" .= prototypeFieldDescription,
            "data_type" .= prototypeFieldType,
            "is_required" .= prototypeFieldIsRequired,
            "is_repeatable" .= prototypeFieldIsRepeatable,
            "deflt" .= prototypeFieldDefault,
            "meta" .= prototypeFieldMeta,
            "placeholder" .= prototypeFieldPlaceholder,
            "position" .= prototypeFieldPosition
        ]

data PrototypeGroup = PrototypeGroup {
    prototypeGroupName :: Maybe String,
    prototypeGroupIterationLimit :: Int,
    prototypeGroupLower :: Int,
    prototypeGroupUpper :: Int
} deriving (Show)

instance Eq PrototypeGroup where
    a == b = (prototypeGroupUpper a) == (prototypeGroupUpper b)

instance Ord PrototypeGroup where
    compare a b = compare (prototypeGroupUpper a) (prototypeGroupUpper b)

instance FromJSON PrototypeGroup where
    parseJSON = withObject "group" $ \o -> do
        prototypeGroupName <- o .: "name"
        prototypeGroupIterationLimit <- o .: "iteration_limit"
        prototypeGroupLower <- o .: "lower"
        prototypeGroupUpper <- o .: "upper"
        return PrototypeGroup{..}

instance ToJSON PrototypeGroup where
    toJSON PrototypeGroup{..} = object $ filterNulls [
            "name" .= prototypeGroupName,
            "iteration_limit" .= prototypeGroupIterationLimit,
            "lower" .= prototypeGroupLower,
            "upper" .= prototypeGroupUpper
        ]

data PrototypeExtra = PrototypeExtra {
    prototypeVersion :: Int,
    prototypeIsCurrent :: Bool,
    prototypeTitle :: String,
    prototypeFields :: Set PrototypeField,
    prototypeGroups :: Set PrototypeGroup 
} deriving (Show)

instance Extra PrototypeExtra where
    extraEntityClass = PROTOTYPE 

instance FromJSON PrototypeExtra where
    parseJSON = withObject "prototype" $ \o -> do
        prototypeVersion <- o .: "version"
        prototypeIsCurrent <- o .: "is_current"
        prototypeTitle <- o .: "title"
        prototypeFields <- fromMaybe Set.empty <$> o .: "fields"
        prototypeGroups <- fromMaybe Set.empty <$> o .: "groups"
        return PrototypeExtra{..}

type Prototype = Entity PrototypeExtra

createPrototypeField :: String -> String -> String -> Int -> PrototypeField
createPrototypeField name description dataType position = PrototypeField {
        prototypeFieldName = name,
        prototypeFieldDescription = description,
        prototypeFieldDefault = Nothing,
        prototypeFieldType = dataType,
        prototypeFieldIsRequired = False,
        prototypeFieldIsRepeatable = False,
        prototypeFieldMeta = defaultMeta,
        prototypeFieldPlaceholder = Nothing,
        prototypeFieldPosition = position
    }

createPrototype :: Identifier Prototype -> String -> String -> Set PrototypeField -> Set PrototypeGroup -> MAS Prototype
createPrototype name title description fields groups = createResource $ object $ filterNulls [
        "name" .= name,
        "title" .= title,
        "description" .= description,
        "fields" .= fields,
        "groups" .= groups
    ]

getPrototype = getResource @Prototype
listPrototypes = listResource @Prototype

getPrototypeVersion :: Identifier Prototype -> Int -> MAS Prototype
getPrototypeVersion identifier version = fmap envelopeFirst $ withResource @Prototype $ withIdentifier identifier $ withOption ("version" =: version) $ get

deletePrototype = deleteResource @Prototype