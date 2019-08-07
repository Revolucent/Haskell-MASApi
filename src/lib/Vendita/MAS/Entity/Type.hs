{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Type (
    TypeExtra(..),
    Type,
    getType,
    isTypeSupported,
    listTypes
)

where

import Data.Aeson
import Data.List
import Data.Maybe
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data TypeExtra = TypeExtra {
    typeProtocols :: [String],
    typeDefinition :: Maybe String,
    typeEnumerations :: [Value],
    typePatterns :: [String]
} deriving (Show)

instance Extra TypeExtra where
    extraEntityClass = TYPE 

instance FromJSON TypeExtra where
    parseJSON = withObject "type" $ \o -> do
        typeProtocols <- fromMaybe [] <$> o .: "protocol"
        typeDefinition <- o .: "definition"
        typeEnumerations <- fromMaybe [] <$> o .: "enumerations"
        typePatterns <- fromMaybe [] <$> o .: "pattern"
        return TypeExtra{..}

type Type = Entity TypeExtra

listTypes :: Bool -> MAS [Type]
listTypes = listResource

getType :: Identifier Type -> MAS Type
getType = getResource

supportedTypes :: [String]
supportedTypes = [
        "boolean",
        "ename",
        "float",
        "datetime",
        "integer",
        "mas0000.sys.email_address",
        "mas0000.sys.phone_number",
        "mas0000.sys.process.process",
        "mas0000.sys.process.variadic_parameter",
        "none",
        "string",
        "unsigned_float",
        "unsigned_integer"
    ]

-- This function may return invalid information if the type is summarized.
isTypeSupported :: Type -> Bool
isTypeSupported typ = 
       (name `elem` supportedTypes) 
    || ("mas0000.sys.account.conn." `isPrefixOf` name) 
    || (not $ null $ typeEnumerations $ entityExtra typ)
    where
        name :: String
        name = let ename = entityName typ in fromMaybe ename $ stripPrefix "optional_" ename
        