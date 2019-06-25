{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Protocol (
    Protocol(..),
    listProtocols
)
where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Vendita.MAS.Resource

data Protocol = Protocol {
    protocolName :: String,
    protocolBase :: Maybe String,
    protocolSpecialRequired :: [String],
    protocolSpecialOptional :: [String],
    protocolDefaultAddress :: Maybe String,
    protocolDefaultPort :: Maybe Int,
    protocolIsAbstract :: Bool,
    protocolAllowsRSAKey :: Bool
} deriving (Show)

instance FromJSON Protocol where
    parseJSON = withObject "protocol" $ \o -> do
        protocolName <- o .: "name"
        protocolBase <- o .: "base"
        protocolSpecialOptional <- fromMaybe [] <$> o .: "special_optional"
        protocolSpecialRequired <- fromMaybe [] <$> o .: "special_required" 
        protocolDefaultAddress <- o .: "default_address"
        protocolDefaultPort <- o .: "default_port"
        protocolIsAbstract <- o .: "is_abstract"
        protocolAllowsRSAKey <- o .: "allow_rsa_key"
        return Protocol{..}

instance Resource Protocol where
    type Identifier Protocol = String
    resourceIdentifier = protocolName
    resourcePathSegment = "protocol"

listProtocols = listResource @Protocol