{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.DataTypes (
    DataType(..),
    getDataType,
    listAllDataTypes
)

where

import Data.Aeson
import Vendita.MAS.Core

data DataType = DataType {
        dataTypeName :: String,
        dataTypeDefinition :: Maybe String
    } deriving (Show, Eq, Ord)

instance FromJSON DataType where
    parseJSON = withObject "DataType" $ \o -> do
        dataTypeName <- o .: "name"
        dataTypeDefinition <- o .: "definition"
        return DataType{..}

instance ToJSON DataType where
    toJSON = toObject [
            "name" .=. dataTypeName,
            "definition" .=. dataTypeDefinition
        ]

instance Resource DataType where
    type Identifier DataType = String
    resourceIdentifier = dataTypeName
    resourcePathSegment = "type"

instance NamedResource DataType where
    resourceName = dataTypeName

listAllDataTypes :: MAS [DataType]
listAllDataTypes = listAllResource

getDataType :: Identifier DataType -> MAS (Maybe DataType)
getDataType = firstResourceWithIdentifier

