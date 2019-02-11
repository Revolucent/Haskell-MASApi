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
    prototypeContent :: Object 
} deriving (Show)

instance Resource Prototype where
    type Identifier Prototype = String
    resourceIdentifier _ = "prototype"
    resourcePathSegment = "prototype"

instance FromJSON Prototype where
    parseJSON = withObject "prototype" $ \o -> do
        let prototypeContent = o
        return Prototype{..}

listAllPrototypes :: MAS [Prototype]
listAllPrototypes = listAllResource

listPrototypes :: [Identifier Prototype] -> MAS [Prototype]
listPrototypes = listResourceWithIdentifiers