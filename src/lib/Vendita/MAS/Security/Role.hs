{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Security.Role (
    Group(..),
    Role(..),
    User(..)
) where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Resource
import Vendita.MAS.Security.Privilege

class (NamedResource r) => Role r where
    roleName :: r -> String
    roleName = resourceName
    roleNamespace :: r -> String
    roleIsGroup :: Bool

data User = User {
    userName :: String,
    userDescription :: String,
    userNamespace :: String,
    userClassPrivileges :: Maybe ClassPrivileges,
    userGroupMemberships :: [Identifier Group],
    userDateCreated :: MASTime,
    userDateUpdated :: MASTime
} deriving (Show)

instance Resource User where
    type Identifier User = String
    resourceIdentifier = userName
    resourceUserOwner = userName
    resourceDateCreated = userDateCreated
    resourceDateUpdated = userDateUpdated
    resourcePathSegment = "user"

instance NamedResource User where
    resourceName = userName

instance Role User where
    roleNamespace = userNamespace
    roleIsGroup = False

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        userName <- o .: "name"
        userDescription <- o .: "description"
        userNamespace <- o .: "fqhome"
        userClassPrivileges <- o .: "class_privileges"
        userGroupMemberships <- o .: "member_of"
        userDateCreated <- o .: "date_created"
        userDateUpdated <- o .: "date_updated"
        return User{..}

data Group = Group {
    groupName :: String,
    groupDescription :: String,
    groupNamespace :: String,
    groupClassPrivileges :: Maybe ClassPrivileges,
    groupDateCreated :: MASTime,
    groupDateUpdated :: MASTime
} deriving (Show)

instance Resource Group where
    type Identifier Group = String 
    resourceIdentifier = groupName
    resourceUserOwner = groupName
    resourceDateCreated = groupDateCreated
    resourceDateUpdated = groupDateUpdated
    resourcePathSegment = "group"

instance NamedResource Group where
    resourceName = groupName

instance Role Group where
    roleNamespace = groupNamespace
    roleIsGroup = True

instance FromJSON Group where
    parseJSON = withObject "group" $ \o -> do
        groupName <- o .: "name"
        groupDescription <- o .: "description"
        groupNamespace <- o .: "fqhome"
        groupClassPrivileges <- o .: "class_privileges"
        groupDateCreated <- o .: "date_created"
        groupDateUpdated <- o .: "date_updated"
        return Group{..}