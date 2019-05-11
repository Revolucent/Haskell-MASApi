{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Form (
    FormExtra(..),
    Form
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Prototype
import Vendita.MAS.Resource

data FormExtra = FormExtra {
    formIsCompleted :: Bool,
    formPrototypeName :: Identifier Prototype, 
    formPrototypeVersion :: Int
} deriving (Show)

instance Extra FormExtra where
    extraEntityClass = FORM 

instance FromJSON FormExtra where
    parseJSON = withObject "form" $ \o -> do
        formIsCompleted <- o .: "is_completed"
        formPrototypeName <- o .: "prototype"
        formPrototypeVersion <- o .: "version"
        return FormExtra{..}

type Form = Entity FormExtra
