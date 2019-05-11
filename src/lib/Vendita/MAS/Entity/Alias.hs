{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Alias (
    AliasExtra(..),
    Alias
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data AliasExtra = AliasExtra {
    aliasedName :: String,
    aliasedClass :: Class
} deriving (Show)

instance Extra AliasExtra where
    extraEntityClass = ALIAS 

instance FromJSON AliasExtra where
    parseJSON = withObject "alias" $ \o -> do 
        aliasedName <- o .: "alias"
        aliasedClass <- o .: "alias_type"
        return AliasExtra{..}

type Alias = Entity AliasExtra
