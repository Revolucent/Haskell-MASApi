{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Account (
    Account,
    AccountExtra(..),
    AccountExtraSpecial(..),
    NewAccount(..),
    accountURL,
    createAccount,
    defaultAccountSpecial,
    deleteAccount,
    getAccount,
    listAccounts,
    modifyAccount
)

where

import Data.Aeson
import Data.Char (isSpace, toLower)
import Data.List (dropWhile, dropWhileEnd)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Printf (printf)
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Attributes hiding ((.=))
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data AccountExtraSpecial = AccountExtraSpecial {
    accountSpecialDatabase :: Maybe String,
    accountSpecialMode :: Maybe String
} deriving (Show)

instance FromJSON AccountExtraSpecial where
    parseJSON = withObject "special" $ \o -> do
        accountSpecialDatabase <- o .:? "database"
        accountSpecialMode <- o .:? "mode"
        return AccountExtraSpecial{..}

instance ToJSON AccountExtraSpecial where
    toJSON AccountExtraSpecial{..} = object $ filterNulls [ "database" .= accountSpecialDatabase, "mode" .= accountSpecialMode ]

defaultAccountSpecial = AccountExtraSpecial {
        accountSpecialDatabase = Nothing,
        accountSpecialMode = Nothing
    }

data AccountExtra = AccountExtra {
    accountAddress :: String,
    accountUser :: String,
    accountUserKey :: Maybe String,
    accountProtocol :: String,
    accountSpecial :: AccountExtraSpecial 
} deriving (Show)

instance Extra AccountExtra where
    extraEntityClass = ACCOUNT

instance FromJSON AccountExtra where
    parseJSON = withObject "account" $ \o -> do
        accountAddress <- o .: "address"
        accountUser <- o .: "user"
        accountUserKey <- o .:? "user_key"
        accountProtocol <- o .: "protocol"
        accountSpecial <- o .: "special"
        return AccountExtra{..}

type Account = Entity AccountExtra

getAccount = getResource @Account
listAccounts = listResource @Account

data NewAccount = NewAccount {
    newAccountName :: String,
    newAccountDescription :: String,
    newAccountProtocol :: String,
    newAccountAddress :: String,
    newAccountPort :: Maybe Int,
    newAccountUser :: String,
    newAccountUserKey :: String,
    newAccountSpecial :: AccountExtraSpecial
} deriving (Show)

instance ToJSON NewAccount where
    toJSON NewAccount{..} = object $ filterNulls [
            "name" .= newAccountName,
            "description" .= newAccountDescription,
            "protocol" .= newAccountProtocol,
            "address" .= newAccountAddress,
            "port" .= newAccountPort,
            "user" .= newAccountUser,
            "user_key" .= newAccountUserKey,
            "special" .= newAccountSpecial
        ]

createAccount :: NewAccount -> MAS Account
createAccount account = createResource account

deleteAccount = deleteResource @Account

modifyAccount :: Identifier Account -> Attributes Account -> MAS Account
modifyAccount name attributes = modifyResource name (object $ toPairs attributes)

accountURL :: Account -> String
accountURL = accountExtraURL . entityExtra 
    where
        accountExtraURL AccountExtra{..} = printf "%s://%s:****@%s%s" (map toLower accountProtocol) accountUser accountAddress (accountExtraSpecialDatabase accountSpecial)
        accountExtraSpecialDatabase :: AccountExtraSpecial -> String 
        accountExtraSpecialDatabase AccountExtraSpecial{..} = case accountSpecialDatabase of 
            Just database -> let trimmedabase = (dropWhile isSpace . dropWhileEnd isSpace) database in 
                                if (length trimmedabase) == 0 then "" else printf "/%s" trimmedabase
            Nothing -> ""
