{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Forms (
    Form (..),
    listAllForms,
    listForms,
    cloneForm
) where

import Control.Monad.Reader
import Data.Aeson
import Data.UUID as UUID
import Data.UUID (UUID)
import Vendita.MAS.Core

data FormValue = FormValue { 
    formValueName :: String, 
    formValueValue :: Value, 
    formValueIsRequired :: Bool,
    formValueDataType :: String
} deriving (Show)

instance FromJSON FormValue where 
    parseJSON = withObject "object" $ \o -> do
        formValueName <- o .: "name"
        formValueValue <- o .: "value"
        formValueIsRequired <- o .: "is_required"
        formValueDataType <- o .: "data_type"
        return FormValue{..}

data Form = Form { 
    formUUID :: UUID, 
    formName :: String, 
    formPrototype :: String, 
    formPrototypeVersion :: Int, 
    formValues :: [FormValue]
} deriving (Show)

instance FromJSON Form where
    parseJSON = withObject "object" $ \o -> do
        formUUID <- o .: "uuid"
        formName <- o .: "name"
        formPrototype <- o .: "prototype"
        formPrototypeVersion <- o .: "version"
        formValues <- o .: "values"
        return Form{..}

instance Resource Form where
    type Identifier Form = UUID
    resourceIdentifier = formUUID
    resourcePathSegment = "form"

newtype FormNewName = FormNewName String

instance ToJSON FormNewName where
    toJSON (FormNewName name) = object [ "name" .= name ]

listForms :: [Identifier Form] -> MAS [Form]
listForms = listResourceWithIdentifiers 

listAllForms :: MAS [Form]
listAllForms = listAllResource 

cloneForm :: Identifier Form -> String -> MAS Form
cloneForm uuid newName = do
    envelope <- withEndpoint @Form $ withIdentifier uuid $ withPath "clone" $ post (FormNewName newName)
    return $ (envelopeContents envelope) !! 0