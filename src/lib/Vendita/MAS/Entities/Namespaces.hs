{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Namespaces (
    Namespace (..),
    listAllNamespaces,
    listNamespaces
) where

import Control.Monad.Reader
import Data.Aeson
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

