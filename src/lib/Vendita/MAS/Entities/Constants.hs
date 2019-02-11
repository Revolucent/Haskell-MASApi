{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Constants (
    Constant (..),
    listAllConstants,
    listConstants
) where

import Control.Monad.Reader
import Data.Aeson
import Data.UUID as UUID
import Data.UUID (UUID)
import Network.HTTP.Req ((=:))
import Vendita.MAS.Core

data Constant = Constant {
    constantName :: String,
    constantValue :: String,
    constantDescription :: String
} deriving (Show)

instance Resource Constant where
    type Identifier Constant = String
    resourceIdentifier = constantName 
    resourcePathSegment = "constant"

instance NamedResource Constant where
    resourceName = constantName

instance DescribedResource Constant where
    resourceDescription = constantDescription

instance FromJSON Constant where
    parseJSON = withObject "object" $ \o -> do
        constantName <- o .: "name"
        constantValue <- o .: "value"
        constantDescription <- o .: "description"
        return Constant{..}

listAllConstants :: MAS [Constant]
listAllConstants = listAllResource

listConstants :: [Identifier Constant] -> MAS [Constant]
listConstants = listResourceWithIdentifiers