{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Security.Privilege (
    ActualPrivileges(..),
    ClassPrivilege(..),
    ClassPrivileges,
    DenyOperator(..),
    EntityPrivilege(..),
    EntityPrivileges,
    GrantOperator(..),
    Operator(..),
    Privileges(..),
    withEntityPrivilege,
    withOperator
)
where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower, toUpper)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (unpack)
import Text.Read (readMaybe)
import Vendita.MAS.Core
import Vendita.MAS.Entity.Class
import Vendita.MAS.Resource

class Pathed a => Operator a

data GrantOperator = GRANT | REVOKE deriving (Eq, Ord, Enum, Read, Show)

instance Operator GrantOperator

instance Pathed GrantOperator where
    pathSegment GRANT = "grant"
    pathSegment REVOKE = "revoke"

data DenyOperator = ALLOW | DENY deriving (Eq, Ord, Enum, Read, Show)

instance Operator DenyOperator

instance Pathed DenyOperator where
    pathSegment ALLOW = "allow"
    pathSegment DENY = "deny"

withOperator :: (MonadReader Connection m, Operator o) => o -> m a -> m a
withOperator = withPath . pathSegment

class Privilege a

data EntityPrivilege = AFFILIATE | EXECUTE | READ | WRITE deriving (Eq, Ord, Enum, Read, Show)

instance Privilege EntityPrivilege

instance Pathed EntityPrivilege where
    pathSegment AFFILIATE = "affiliate"
    pathSegment EXECUTE = "execute"
    pathSegment READ = "read"
    pathSegment WRITE = "write"

instance FromJSON EntityPrivilege where
    parseJSON invalid@(String privilege) = case readMaybe (map toUpper $ unpack privilege) of
        Just privilege -> return privilege
        _ -> typeMismatch "Invalid value for EntityPrivilege" invalid
    parseJSON invalid = typeMismatch "Invalid value for EntityPrivilege" invalid

instance ToJSON EntityPrivilege where
    toJSON = toJSON . map toLower . show

withEntityPrivilege :: (MonadReader Connection m) => EntityPrivilege -> m a -> m a
withEntityPrivilege = withPath . pathSegment

data ClassPrivilege = CREATE deriving (Eq, Ord, Enum, Read, Show)

instance Privilege ClassPrivilege

instance Pathed ClassPrivilege where
    pathSegment CREATE = "create"

instance FromJSON ClassPrivilege where
    parseJSON invalid@(String privilege) = case readMaybe (map toUpper $ unpack privilege) of
        Just privilege -> return privilege
        _ -> typeMismatch "Invalid value for ClassPrivilege" invalid
    parseJSON invalid = typeMismatch "Invalid value for ClassPrivilege" invalid

instance ToJSON ClassPrivilege where
    toJSON = toJSON . map toLower . show 

data ActualPrivileges a = ActualPrivileges {
    actualPrivilegesGranted :: a, 
    actualPrivilegesDenied :: a 
} deriving (Show)

instance (FromJSON a) => FromJSON (ActualPrivileges a) where
    parseJSON = withObject "actual privileges" $ \o -> do
        actualPrivilegesGranted <- o .: "granted"
        actualPrivilegesDenied <- o .: "denied"
        return ActualPrivileges{..}

data Privileges a = Privileges {
    actualPrivileges :: ActualPrivileges a,
    effectivePrivileges :: a 
} deriving (Show)

instance (FromJSON a) => FromJSON (Privileges a) where
    parseJSON = withObject "privileges" $ \o -> do
        actualPrivileges <- o .: "actual"
        effectivePrivileges <- o .: "effective"
        return Privileges{..}

type EntityPrivileges = Privileges (Set EntityPrivilege)
type ClassPrivileges = Privileges (Map Class (Set ClassPrivilege))
