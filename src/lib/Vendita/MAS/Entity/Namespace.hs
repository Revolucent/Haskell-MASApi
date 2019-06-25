{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Namespace (
    NamespaceExtra(..),
    Namespace,
    createNamespace,
    deleteNamespace,
    getNamespace,
    listNamespaces,
    modifyNamespace
)

where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data NamespaceExtra = NamespaceExtra deriving (Show)

instance Extra NamespaceExtra where
    extraEntityClass = NAMESPACE 

instance FromJSON NamespaceExtra where
    parseJSON _ = return NamespaceExtra

type Namespace = Entity NamespaceExtra

listNamespaces = listResource @Namespace
getNamespace = getResource @Namespace

createNamespace :: Identifier Namespace -> String -> MAS Namespace
createNamespace name description = createResource $ object [ "name" .= name, "description" .= description ] 

modifyNamespace :: Identifier Namespace -> String -> MAS Namespace
modifyNamespace name rename = modifyResource name $ object [ "rename" .= rename ]

deleteNamespace :: Identifier Namespace -> MAS ()
deleteNamespace = deleteResource @Namespace