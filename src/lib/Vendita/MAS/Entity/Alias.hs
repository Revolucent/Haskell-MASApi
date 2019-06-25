{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Alias (
    AliasExtra(..),
    Alias,
    createAlias,
    deleteAlias,
    getAlias,
    listAliases
)

where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource
import Vendita.MAS.Diagnostics (printPretty)

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

deleteAlias = deleteResource @Alias
getAlias = getResource @Alias
listAliases = listResource @Alias

createAlias :: String -> String -> Class -> String -> MAS Alias
createAlias name description cls to = createResource $ object [
        "name" .= to, -- Yes, really. It's reversed.
        "description" .= description,
        "alias" .= name -- WTF?
    ] 