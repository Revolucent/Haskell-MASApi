{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Accounts (
    Account(..),
    Protocol(..),
    createAccount,
    getAccount,
    listAllAccounts,
    listAccounts
)
where

import Data.Aeson
import Data.Text (pack, unpack)
import Network.HTTP.Req ((=:))
import Vendita.MAS.Core

data Protocol = SSH | ORACLE | MYSQL deriving (Eq, Ord, Enum, Show, Read)

instance FromJSON Protocol where
    parseJSON (String p) = fmap read $ pure $ unpack p 
    parseJSON _ = error "Invalid protocol"

instance ToJSON Protocol where 
    toJSON p = String (pack $ show p)

data Account = Account {
    accountName :: String,
    accountDescription :: String,
    accountProtocol :: Protocol,
    accountUser :: String,
    accountUserKey :: Maybe String,
    accountAddress :: String,
    accountPort :: Maybe Int,
    accountSpecial :: Object
} deriving (Show)

instance FromJSON Account where
    parseJSON = withObject "account" $ \o -> do 
        accountName <- o .: "name"
        accountDescription <- o .: "description"
        accountProtocol <- o .: "protocol"
        accountUser <- o .: "user"
        accountUserKey <- o .: "user_key"
        accountAddress <- o .: "address"
        accountPort <- o .: "port"
        accountSpecial <- o .: "special"
        return Account{..}

instance ToJSON Account where
    toJSON account = object [
            "name" .= (accountName account),
            "description" .= (accountDescription account),
            "protocol" .= (accountProtocol account),
            "user" .= (accountUser account),
            "user_key" .= (accountUserKey account),
            "address" .= (accountAddress account),
            "port" .= (accountPort account),
            "special" .= (accountSpecial account)
        ]

instance Resource Account where
    type Identifier Account = String
    resourceIdentifier = accountName 
    resourcePathSegment = "account"

instance NamedResource Account where
    resourceName = accountName 

instance DescribedResource Account where
    resourceDescription = accountDescription 

listAllAccounts :: MAS [Account] 
listAllAccounts = listAllResource

listAccounts :: [Identifier Account] -> MAS [Account]
listAccounts = listResourceWithIdentifiers 

getAccount :: Identifier Account -> MAS (Maybe Account)
getAccount = firstResourceWithIdentifier

createAccount :: Account -> MAS Account
createAccount account = fmap envelopeFirst $ withEndpoint @Account $ post account