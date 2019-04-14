{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Namespaces (
    Namespace (..),
    createNamespace,
    deleteNamespace,
    deleteNamespaces,
    getNamespace,
    listAllNamespaces,
    listNamespaces,
    modifyNamespace
) where

import Control.Monad.Reader
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.UUID as UUID
import Data.UUID (UUID)
import Vendita.MAS.Core

data Namespace = Namespace { 
    namespaceName :: String,
    namespaceDescription :: String
} deriving (Show)

instance FromJSON Namespace where
    parseJSON = withObject "object" $ \o -> do
        namespaceName <- o .: "name"
        namespaceDescription <- o .: "description"
        return Namespace{..}

instance ToJSON Namespace where
    toJSON ns = object [ "name" .= (namespaceName ns), "description" .= (namespaceDescription ns) ]

instance Resource Namespace where
    type Identifier Namespace = String
    resourceIdentifier = namespaceName
    resourcePathSegment = "namespace"

instance NamedResource Namespace where
    resourceName = namespaceName

instance DescribedResource Namespace where
    resourceDescription = namespaceDescription

listAllNamespaces :: MAS [Namespace]
listAllNamespaces = listAllResource 

listNamespaces :: [Identifier Namespace] -> MAS [Namespace]
listNamespaces = listResourceWithIdentifiers 

getNamespace :: Identifier Namespace -> MAS (Maybe Namespace)
getNamespace = firstResourceWithIdentifier

createNamespace :: String -> String -> MAS Namespace
createNamespace name description = fmap envelopeFirst $ withEndpoint @Namespace $ post Namespace { namespaceName = name, namespaceDescription = description } 

modifyNamespace :: String -> String -> String -> MAS Namespace
modifyNamespace name newName newDescription = fmap envelopeFirst $ withEndpoint @Namespace $ withPath (pack name) $ patch Namespace { namespaceName = newName, namespaceDescription = newDescription } 

deleteNamespaces :: [Identifier Namespace] -> MAS ()
deleteNamespaces = deleteResourceWithIdentifiers_ @Namespace

deleteNamespace :: Identifier Namespace -> MAS ()
deleteNamespace ns = deleteNamespaces [ns]
