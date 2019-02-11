{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Namespaces (
    Namespace (..),
    listAllNamespaces,
    listNamespaces,
    createNamespace,
    modifyNamespace,
    deleteNamespace_,
    deleteNamespaces_
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

createNamespace :: String -> String -> MAS Namespace
createNamespace name description = fmap envelopeFirst $ withEndpoint @Namespace $ post Namespace { namespaceName = name, namespaceDescription = description } 

modifyNamespace :: String -> String -> String -> MAS Namespace
modifyNamespace name newName newDescription = fmap envelopeFirst $ withEndpoint @Namespace $ withPath (pack name) $ patch Namespace { namespaceName = newName, namespaceDescription = newDescription } 

deleteNamespaces_ :: [Identifier Namespace] -> MAS ()
deleteNamespaces_ = deleteResourceWithIdentifiers_ @Namespace

deleteNamespace_ :: Identifier Namespace -> MAS ()
deleteNamespace_ ns = deleteNamespaces_ [ns]
