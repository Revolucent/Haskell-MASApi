{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Prototypes (
    Prototype (..),
    listAllPrototypes,
    listPrototypes
) where

import Control.Monad.Reader
import Data.Aeson
import Data.UUID as UUID
import Data.UUID (UUID)
import Vendita.MAS.Core

data Prototype = Prototype { 
    prototypeName :: String,
    prototypeTitle :: String,
    prototypeDescription :: String
} deriving (Show)

instance Resource Prototype where
    type Identifier Prototype = String
    resourceIdentifier = prototypeName 
    resourcePathSegment = "prototype"

instance NamedResource Prototype where 
    resourceName = prototypeName

instance DescribedResource Prototype where
    resourceDescription = prototypeDescription

instance FromJSON Prototype where
    parseJSON = withObject "prototype" $ \o -> do
        prototypeName <- o .: "name"
        prototypeTitle <- o .: "title"
        prototypeDescription <- o .: "description"
        return Prototype{..}

listAllPrototypes :: MAS [Prototype]
listAllPrototypes = listAllResource

listPrototypes :: [Identifier Prototype] -> MAS [Prototype]
listPrototypes = listResourceWithIdentifiers