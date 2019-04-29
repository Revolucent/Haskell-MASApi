{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Constants (
    Constant (..),
    ChangeConstant (..),
    deleteConstant,
    emptyChangeConstant,
    listAllConstants,
    listConstants,
    createConstant,
    patchConstant
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

instance ToJSON Constant where
    toJSON Constant{..} = object [
            "name" .= constantName,
            "value" .= constantValue,
            "description" .= constantDescription
        ]

data ChangeConstant = ChangeConstant {
    changeConstantRename :: Maybe String,
    changeConstantValue :: Maybe String,
    changeConstantDescription :: Maybe String
} deriving (Show)

emptyChangeConstant = ChangeConstant { changeConstantRename = Nothing, changeConstantValue = Nothing, changeConstantDescription = Nothing }

instance ToJSON ChangeConstant where
    toJSON ChangeConstant{..} = object $ filterNulls [
            "rename" .= changeConstantRename,
            "value" .= changeConstantValue,
            "description" .= changeConstantDescription
        ]

listAllConstants :: MAS [Constant]
listAllConstants = listAllResource

listConstants :: [Identifier Constant] -> MAS [Constant]
listConstants = listResourceWithIdentifiers

createConstant :: Constant -> MAS () 
createConstant constant = withResource @Constant $ post_ constant

patchConstant :: Identifier Constant -> ChangeConstant -> MAS Constant
patchConstant name constant = fmap envelopeFirst $ withResource @Constant $ withIdentifier name $ patch constant

deleteConstant :: Identifier Constant -> MAS ()
deleteConstant = deleteResourceWithIdentifier_ @Constant