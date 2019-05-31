{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Meta (
    Meta(..)
)
where

import Data.Aeson
import Vendita.MAS.Core

data Meta = Meta {
    metaEditor :: Maybe String
} deriving (Show)

instance FromJSON Meta where
    parseJSON = withObject "meta" $ \o -> do 
        metaEditor <- o .:? "editor"
        return Meta{..}