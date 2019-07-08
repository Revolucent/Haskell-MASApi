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
    metaProtocol :: Maybe String,
    metaPosition :: Maybe Int
} deriving (Show)

defaultMeta = Meta { metaEditor = Nothing, metaProtocol = Nothing, metaPosition = Nothing }

instance FromJSON Meta where
    parseJSON = withObject "meta" $ \o -> do 
        metaEditor <- o .:? "editor"
        metaPosition <- o .:? "position"
        metaProtocol <- o .:? "protocol"
        return Meta{..}

instance ToJSON Meta where
    toJSON Meta{..} = object $ filterNulls [ "editor" .= metaEditor, "protocol" .= (map toUpper <$> metaProtocol), "position" .= metaPosition ]