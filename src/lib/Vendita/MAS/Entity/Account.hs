{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Account (
    AccountAttribute(..),
    AccountExtra(..),
    Account,
    getAccount,
    listAccounts,
    modifyAccount
)

where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

{-
special : object
-}

data AccountExtra = AccountExtra {
    accountAddress :: String,
    accountProtocol :: String,
    accountSpecial :: Object 
} deriving (Show)

instance Extra AccountExtra where
    extraEntityClass = ACCOUNT

instance FromJSON AccountExtra where
    parseJSON = withObject "account" $ \o -> do
        accountAddress <- o .: "address"
        accountProtocol <- o .: "protocol"
        accountSpecial <- o .: "special"
        return AccountExtra{..}

type Account = Entity AccountExtra

getAccount = getResource @Account
listAccounts = listResource @Account

data AccountAttribute = AccountRename String | 
                        AccountDescription String | 
                        AccountProtocol String | 
                        AccountUserKey String |
                        AccountUser String |  
                        AccountAddress String |
                        AccountPort Int

instance NamedAttribute AccountAttribute where 
    attributeName (AccountRename _) = "rename"
    attributeName (AccountDescription _) = "description"
    attributeName (AccountProtocol _) = "protocol"
    attributeName (AccountUserKey _) = "user_key"
    attributeName (AccountUser _) = "user"
    attributeName (AccountAddress _) = "address"
    attributeName (AccountPort _) = "port"

instance ToJSON AccountAttribute where
    toJSON (AccountRename rename) = toJSON rename
    toJSON (AccountDescription description) = toJSON description
    toJSON (AccountProtocol protocol) = toJSON protocol
    toJSON (AccountUserKey userKey) = toJSON userKey
    toJSON (AccountUser user) = toJSON user
    toJSON (AccountAddress address) = toJSON address
    toJSON (AccountPort port) = toJSON port

modifyAccount :: Identifier Account -> [AccountAttribute] -> MAS Account
modifyAccount = modifyResourceAttributes