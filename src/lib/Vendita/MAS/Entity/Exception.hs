{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Exception (
    Exception,
    ExceptionExtra(..),
    ExceptionMessageAttribute(..),
    createException,
    deleteException,
    getException,
    listExceptions,
    modifyException
)

where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource
import Vendita.MAS.Strings

data ExceptionExtra = ExceptionExtra { exceptionMessage :: String } deriving (Show)

instance Extra ExceptionExtra where
    extraEntityClass = EXCEPTION 

instance FromJSON ExceptionExtra where
    parseJSON = withObject "exception" $ \o -> do 
        exceptionMessage <- o .: "message"
        return ExceptionExtra{..}

type Exception = Entity ExceptionExtra

data ExceptionMessageAttribute = ExceptionMessageAttribute

instance ToText ExceptionMessageAttribute where
    toText _ = "message"

instance AttributeKey ExceptionMessageAttribute where
    type AttributeValue ExceptionMessageAttribute = String
    type AttributeEntity ExceptionMessageAttribute = Exception

createException :: Identifier Exception -> String -> String -> MAS Exception
createException name description message = createResource @Exception $ object [
        "name" .= name,
        "description" .= description,
        "message" .= message
    ] 

modifyException :: Identifier Exception -> Attributes Exception -> MAS Exception
modifyException name attributes = modifyResource @Exception name $ object $ toPairs attributes

getException = getResource @Exception
listExceptions = listResource @Exception
deleteException = deleteResource @Exception