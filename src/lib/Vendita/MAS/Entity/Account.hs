{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Account (
    AccountExtra(..),
    Account
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data AccountExtra = AccountExtra {
    accountAddress :: String
} deriving (Show)

instance Extra AccountExtra where
    extraEntityClass = ACCOUNT

instance FromJSON AccountExtra where
    parseJSON = withObject "account" $ \o -> do
        accountAddress <- o .: "address"
        return AccountExtra{..}

type Account = Entity AccountExtra
