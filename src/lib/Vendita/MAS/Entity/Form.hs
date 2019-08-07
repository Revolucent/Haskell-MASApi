{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Form (
    Form,
    FormExtra(..),
    FormValue(..),
    createForm,
    deleteForm,
    getForm,
    listForms
)

where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Prototype
import Vendita.MAS.Resource

data FormValue = FormValue {
    formValueName :: String,
    formValueDescription :: String,
    formValueType :: String,
    formValueValue :: Value,
    formValueIsRepeatable :: Bool,
    formValueIsGrouped :: Bool,
    formValueGroupLast :: Maybe Int,
    formValuePosition :: Int
} deriving (Show)

instance Eq FormValue where
    a == b = (formValueName a) == (formValueName b)

instance Ord FormValue where
    compare a b = compare (formValueName a) (formValueName b)

instance FromJSON FormValue where
    parseJSON = withObject "value" $ \o -> do
        formValueName <- o .: "name"
        formValueDescription <- o .: "description"
        formValueType <- o .: "data_type"
        formValueValue <- o .: "value"
        formValueIsRepeatable <- o .: "is_repeatable"
        formValueIsGrouped <- o .: "is_grouped"
        formValueGroupLast <- o .: "group_last"
        formValuePosition <- o .: "position"
        return FormValue{..}

data FormExtra = FormExtra {
    formIsCompleted :: Bool,
    formPrototypeName :: Identifier Prototype, 
    formPrototypeVersion :: Int,
    formValues :: Set FormValue 
} deriving (Show)

instance Extra FormExtra where
    extraEntityClass = FORM 

instance FromJSON FormExtra where
    parseJSON = withObject "form" $ \o -> do
        formIsCompleted <- o .: "is_completed"
        formPrototypeName <- o .: "prototype"
        formPrototypeVersion <- o .: "version"
        formValues <- fromMaybe Set.empty <$> o .: "values"
        return FormExtra{..}

type Form = Entity FormExtra

getForm = getResource @Form
listForms = listResource @Form
deleteForm = deleteResource @Form

createForm :: Identifier Form -> Identifier Prototype -> String -> MAS Form
createForm name prototype description = createResource @Form $ object [ "name" .= name, "prototype" .= prototype, "description" .= description ]