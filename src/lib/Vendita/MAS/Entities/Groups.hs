{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Groups (
    Group(..),
    createGroup,
    getGroup,
    grantMembershipInGroupsToGroup,
    listAllGroups,
    revokeMembershipInGroupsFromGroup
)

where

import Data.Aeson
import Vendita.MAS.Core

data Group = Group {
    groupName :: String,
    groupHome :: String,
    groupGroups :: [String],
    groupUsers :: [String]
} deriving (Eq, Ord, Show)

instance Resource Group where
    type Identifier Group = String
    resourceIdentifier = groupName
    resourcePathSegment = "group"

instance FromJSON Group where
    parseJSON = withObject "Group" $ \o -> do
        groupName <- o .: "name"
        groupHome <- o .: "fqhome"
        groupGroups <- o .: "groups"
        groupUsers <- o .: "users"
        return Group{..}

instance ToJSON Group where
    toJSON = toObject [
            "name" .=. groupName,
            "fqhome" .=. groupHome,
            "groups" .=. groupGroups,
            "users" .=. groupUsers
        ]

listAllGroups :: MAS [Group]
listAllGroups = listAllResource

getGroup :: Identifier Group -> MAS (Maybe Group)
getGroup = firstResourceWithIdentifier

data Grant = Grant {
    grantGroups :: [String]
}

instance ToJSON Grant where
    toJSON = toObject [ "groups" .=. grantGroups ]

grantMembershipInGroupsToGroup :: Identifier Group -> [Identifier Group] -> MAS Group
grantMembershipInGroupsToGroup name groups = do 
    let grant = Grant { grantGroups = groups }
    fmap envelopeFirst $ withEndpoint @Group $ withIdentifier name $ withPath "grant" $ patch grant

revokeMembershipInGroupsFromGroup :: Identifier Group -> [Identifier Group] -> MAS Group
revokeMembershipInGroupsFromGroup name groups = do
    let grant = Grant { grantGroups = groups }
    fmap envelopeFirst $ withEndpoint @Group $ withIdentifier name $ withPath "revoke" $ patch grant

data NewGroup = NewGroup {
        newGroupName :: String,
        newGroupDescription :: String,
        newGroupGroups :: [String]
    }

instance ToJSON NewGroup where
    toJSON = toObject [ "name" .=. newGroupName, "description" .=. newGroupDescription, "groups" .=. newGroupGroups ]

createGroup :: String -> String -> [String] -> MAS Group
createGroup name description groups = do
    let group = NewGroup {
            newGroupName = name,
            newGroupDescription = description,
            newGroupGroups = groups
        }
    fmap envelopeFirst $ withEndpoint @Group $ post group