{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Type (
    TypeExtra(..),
    Type,
    listTypes
)

where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data TypeExtra = TypeExtra {
    typeProtocols :: Maybe [String],
    typeDefinition :: Maybe String,
    typeEnumerations :: Maybe [Value]
} deriving (Show)

instance Extra TypeExtra where
    extraEntityClass = TYPE 

instance FromJSON TypeExtra where
    parseJSON = withObject "type" $ \o -> do
        typeProtocols <- o .: "protocol"
        typeDefinition <- o .: "definition"
        typeEnumerations <- o .: "enumerations"
        return TypeExtra{..}

type Type = Entity TypeExtra

listTypes :: Bool -> MAS [Type]
listTypes = listResource