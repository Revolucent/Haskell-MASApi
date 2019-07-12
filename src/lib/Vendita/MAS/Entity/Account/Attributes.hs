{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Account.Attributes (
    address,
    port,
    protocol,
    special,
    user,
    user_key
)
where

import Vendita.MAS.Entity.Account
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Strings

data AccountStringAttributes = AccountProtocolAttribute | AccountUserKeyAttribute | AccountUserAttribute | AccountAddressAttribute

instance ToText AccountStringAttributes where
    toText AccountProtocolAttribute = "protocol"
    toText AccountUserKeyAttribute = "user_key"
    toText AccountUserAttribute = "user"
    toText AccountAddressAttribute = "address"

instance AttributeKey AccountStringAttributes where
    type AttributeValue AccountStringAttributes = String 
    type AttributeEntity AccountStringAttributes = Account

data AccountPortAttribute = AccountPortAttribute

instance ToText AccountPortAttribute where
    toText _ = "port"

instance AttributeKey AccountPortAttribute where
    type AttributeValue AccountPortAttribute = Int
    type AttributeEntity AccountPortAttribute = Account 

data AccountSpecialAttribute = AccountSpecialAttribute

instance ToText AccountSpecialAttribute where
    toText _ = "special"

instance AttributeKey AccountSpecialAttribute where
    type AttributeValue AccountSpecialAttribute = AccountExtraSpecial
    type AttributeEntity AccountSpecialAttribute = Account

protocol = AccountProtocolAttribute
user_key = AccountUserKeyAttribute
user = AccountUserAttribute
address = AccountAddressAttribute
port = AccountPortAttribute
special = AccountSpecialAttribute