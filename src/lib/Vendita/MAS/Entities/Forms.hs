{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Forms (
    Form (..),
    FormValue (..),
    NewForm (..),
    NewValue (..),
    cloneForm,
    createForm,
    deleteForm,
    deleteForms,
    getForm,
    listAllForms,
    listForms,
    makeForm
) where

import Control.Monad.Reader
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    formName :: String, 
    formDescription :: String,
    formPrototype :: String, 
    formPrototypeVersion :: Int, 
    formValues :: [FormValue]
} deriving (Show)

instance FromJSON Form where
    parseJSON = withObject "object" $ \o -> do
        formName <- o .: "name"
        formDescription <- o .: "description"
        formPrototype <- o .: "prototype"
        formPrototypeVersion <- o .: "version"
        formValues <- o .: "values"
        return Form{..}

instance Resource Form where
    type Identifier Form = String 
    resourceIdentifier = formName 
    resourcePathSegment = "form"

instance NamedResource Form where
    resourceName = formName 

newtype FormNewName = FormNewName String

instance ToJSON FormNewName where
    toJSON (FormNewName name) = object [ "name" .= name ]

data NewValue = NewValue String Value deriving (Show)

instance ToJSON NewValue where 
    toJSON (NewValue name value) = object [ "name" .= name, "value" .= value ]

data NewForm = NewForm {
    newFormName :: String,
    newFormDescription :: String,
    newFormPrototype :: String,
    newFormValues :: [NewValue]
} deriving (Show)

instance ToJSON NewForm where
    toJSON newForm = object [
            "name" .= (newFormName newForm),
            "description" .= (newFormDescription newForm),
            "prototype" .= (newFormPrototype newForm),
            "values" .= (newFormValues newForm)
        ]

makeForm :: String -> String -> String -> Map String Value -> NewForm
makeForm prototype name description values = NewForm {
        newFormName = name,
        newFormDescription = description,
        newFormPrototype = prototype,
        newFormValues = map (\(key, value) -> NewValue key value) $ Map.assocs values   
    }

listForms :: [Identifier Form] -> MAS [Form]
listForms = listResourceWithIdentifiers 

listAllForms :: MAS [Form]
listAllForms = listAllResource 

getForm :: Identifier Form -> MAS (Maybe Form)
getForm = firstResourceWithIdentifier

cloneForm :: Identifier Form -> String -> MAS Form
cloneForm oldName newName = do
    envelope <- withEndpoint @Form $ withIdentifier oldName $ withPath "clone" $ post (FormNewName newName)
    return $ (envelopeContents envelope) !! 0

createForm :: NewForm -> MAS Form
createForm newForm = do
    envelope <- withEndpoint @Form $ post newForm
    return $ (envelopeContents envelope) !! 0

deleteForms :: [Identifier Form] -> MAS ()
deleteForms = deleteResourceWithIdentifiers_ @Form

deleteForm :: Identifier Form -> MAS ()
deleteForm forms = deleteForms [forms]
