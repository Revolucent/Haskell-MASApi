{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Security.Role (
    Group(..),
    Role(..),
    User(..),
    createGroup,
    createUser,
    deleteGroup,
    deleteUser,
    getGroup,
    getUser,
    listGroups,
    listUsers,
    modifyMembership,
    modifyUser
) where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Resource
import Vendita.MAS.Security.Privilege

class Resource r => Role r where
    roleName :: r -> String
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
    resourcePathSegment = "user"

instance Role User where
    roleName = userName
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

getUser = getResource @User
deleteUser = deleteResource @User
listUsers = listResource @User

createRole :: forall r. (Role r, Resource r, FromJSON r, ToJSON (Identifier r)) => Identifier r -> Maybe String -> String -> [Identifier Group] -> MAS r
createRole name password description groups = createResource @r $ object $ filterNulls [
        "name" .= name,
        "password" .= password,
        "description" .= description,
        "groups" .= groups
    ]

createUser name password description groups = createRole @User name (Just password) description groups

modifyUser :: Identifier User -> Maybe String -> Maybe String -> MAS User
modifyUser name password description = modifyResource @User name $ object $ filterNulls [
        "password" .= password,
        "description" .= description
    ]

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
    resourcePathSegment = "group"

instance Role Group where
    roleName = groupName
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

getGroup = getResource @Group
deleteGroup = deleteResource @Group
listGroups = listResource @Group
createGroup name description groups = createRole @Group name Nothing description groups

modifyMembership :: forall r. (Role r, Resource r, FromJSON r, Pathed (Identifier r)) => Identifier r -> GrantOperator -> [Identifier Group] -> MAS r
modifyMembership role op groups = fmap envelopeFirst $ withEndpoint @r $ withIdentifier role $ withOperator op $ patch $ object [
        "groups" .= groups
    ] 