{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Meta (
    Meta(..),
    defaultMeta
)
where

import Data.Aeson
import Data.Char (toUpper)
import Vendita.MAS.Core

data Meta = Meta {
    metaEditor :: Maybe String,
    metaProtocol :: Maybe String
} deriving (Show)

defaultMeta = Meta { metaEditor = Nothing, metaProtocol = Nothing }

instance FromJSON Meta where
    parseJSON = withObject "meta" $ \o -> do 
        metaEditor <- o .:? "editor"
        metaProtocol <- o .:? "protocol"
        return Meta{..}

instance ToJSON Meta where
    toJSON Meta{..} = object $ filterNulls [ "editor" .= metaEditor, "protocol" .= (map toUpper <$> metaProtocol) ]