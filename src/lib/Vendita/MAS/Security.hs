{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Security (
    ActualClassPrivileges(..),
    ClassPrivileges(..),
    Group(..),
    Operator(..),
    Privilege(..),
    User(..),
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower, toUpper)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Vendita.MAS.Core

data User = User {
    userName :: String,
    userDescription :: String,
    userGroups :: [Identifier Group], -- member_of
    userPrivileges :: Maybe ClassPrivileges
} deriving (Read)

instance Resource User where
    type Identifier User = String
    resourceIdentifier = userName
    resourcePathSegment = "user"

instance ToJSON User where
    toJSON User{..} = object [
            "name" .= userName,
            "description" .= userDescription,
            "member_of" .= userGroups,
            "class_privileges" .= userPrivileges
        ]

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        userName <- o .: "name"
        userDescription <- o .: "description"
        userGroups <- o .: "member_of"
        userPrivileges <- o .: "class_privileges"
        return User{..}

data Group = Group {
    groupName :: String,
    groupDescription :: String,
    groupUsers :: [Identifier User],
    groupParentGroups :: [Identifier Group], -- member_of
    groupGroups :: [Identifier Group], -- groups
    groupPrivileges :: Maybe ClassPrivileges
} deriving (Read)

instance Resource Group where
    type Identifier Group = String
    resourceIdentifier = groupName
    resourcePathSegment = "group"

instance ToJSON Group where
    toJSON Group{..} = object [
            "name" .= groupName,
            "description" .= groupDescription,
            "users" .= groupUsers,
            "groups" .= groupParentGroups,
            "member_of" .= groupGroups,
            "class_privileges" .= groupPrivileges
        ]

instance FromJSON Group where
    parseJSON = withObject "Group" $ \o -> do
        groupName <- o .: "name"
        groupDescription <- o .: "description"
        groupUsers <- o .: "users"
        groupParentGroups <- o .: "groups"
        groupGroups <- o .: "member_of"
        groupPrivileges <- o .: "class_privileges"
        return Group{..}

data Privilege = EXECUTE | READ | WRITE | CREATE deriving (Eq, Ord, Enum, Read, Show)

instance EnumerationKey Privilege

instance ToJSON Privilege where
    toJSON = enumerationToJSON

instance ToJSONKey Privilege where
    toJSONKey = enumerationToJSONKey
   
instance FromJSON Privilege where
    parseJSON = enumerationFromJSON

instance FromJSONKey Privilege where
    fromJSONKey = enumerationFromJSONKey

data Operator = GRANT | ALLOW | DENY | REVOKE deriving (Eq, Ord, Enum, Read, Show)

instance EnumerationKey Operator

instance ToJSON Operator where
    toJSON = enumerationToJSON

instance ToJSONKey Operator where
    toJSONKey = enumerationToJSONKey
   
instance FromJSON Operator where
    parseJSON = enumerationFromJSON

instance FromJSONKey Operator where
    fromJSONKey = enumerationFromJSONKey

data ActualClassPrivileges = ActualClassPrivileges {
    actualClassPrivilegesGranted :: Map Entity (Set Privilege),
    actualClassPrivilegesDenied :: Map Entity (Set Privilege)
} deriving (Read)

instance ToJSON ActualClassPrivileges where
    toJSON ActualClassPrivileges{..} = object [
            "granted" .= actualClassPrivilegesGranted,
            "denied" .= actualClassPrivilegesDenied
        ]

instance FromJSON ActualClassPrivileges where
    parseJSON = withObject "ActualClassPrivileges" $ \o -> do
        actualClassPrivilegesGranted <- o .: "granted"
        actualClassPrivilegesDenied <- o .: "denied"
        return ActualClassPrivileges{..}

data ClassPrivileges = ClassPrivileges {
    actualClassPrivileges :: ActualClassPrivileges,
    effectiveClassPrivileges :: Map Entity (Set Privilege)
} deriving (Read)

instance ToJSON ClassPrivileges where
    toJSON ClassPrivileges{..} = object [
            "actual" .= actualClassPrivileges,
            "effective" .= effectiveClassPrivileges
        ]

instance FromJSON ClassPrivileges where
    parseJSON = withObject "ClassPrivileges" $ \o -> do
        actualClassPrivileges <- o .: "actual"
        effectiveClassPrivileges <- o .: "effective"
        return ClassPrivileges{..}