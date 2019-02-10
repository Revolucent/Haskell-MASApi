{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Namespaces (
    Namespace (..),
    withNamespaceEndpoint,
    listAllNamespaces,
    listNamespaces
) where

import Control.Monad.Reader
import Data.Aeson
import Data.UUID as UUID
import Data.UUID (UUID)
import Vendita.MAS.Core

data Namespace = Namespace { namespaceName :: String } deriving (Show)

instance FromJSON Namespace where
    parseJSON = withObject "object" $ \o -> do
        namespaceName <- o .: "name"
        return Namespace{..}

instance Resource Namespace where
    type Identifier Namespace = String
    resourceIdentifier = namespaceName
    resourcePathSegment = "namespace"

withNamespaceEndpoint :: (MonadReader Connection m) => m a -> m a
withNamespaceEndpoint = withPath (resourcePathSegment @Namespace)

listAllNamespaces :: MAS [Namespace]
listAllNamespaces = withNamespaceEndpoint listAll

listNamespaces :: [Identifier Namespace] -> MAS [Namespace]
listNamespaces = withNamespaceEndpoint . listWithIdentifiers

