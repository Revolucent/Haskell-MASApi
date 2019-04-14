{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Users (
    User(..),
    listAllUsers
)

where

{-
"class_privileges": {
            "actual": {
                "granted": {
                    "EXCEPTION": [
                        "create"
                    ],
                    "SCHEDULE": [
                        "create"
                    ],
                    "PROCESS": [
                        "create"
                    ],
                    "NAMESPACE": [
                        "create"
                    ],
                    "ALIAS": [
                        "create"
                    ],
                    "PROTOTYPE": [
                        "create"
                    ],
                    "ACCOUNT": [
                        "create"
                    ],
                    "TYPE": [
                        "create"
                    ],
                    "FORM": [
                        "create"
                    ],
                    "CONSTANT": [
                        "create"
                    ]
                },
                "denied": {
                    "EXCEPTION": [],
                    "SCHEDULE": [],
                    "PROCESS": [],
                    "NAMESPACE": [],
                    "ALIAS": [],
                    "PROTOTYPE": [],
                    "ACCOUNT": [],
                    "TYPE": [],
                    "FORM": [],
                    "CONSTANT": []
                }
            },
            "effective": {
                "EXCEPTION": [
                    "create"
                ],
                "SCHEDULE": [
                    "create"
                ],
                "PROCESS": [
                    "create"
                ],
                "NAMESPACE": [
                    "create"
                ],
                "ALIAS": [
                    "create"
                ],
                "PROTOTYPE": [
                    "create"
                ],
                "ACCOUNT": [
                    "create"
                ],
                "TYPE": [
                    "create"
                ],
                "FORM": [
                    "create"
                ],
                "CONSTANT": [
                    "create"
                ]
            }
        },
-}
    
import Data.Aeson
import Vendita.MAS.Core

data User = User {
    userName :: String,
    userHome :: String,
    userOwner :: String,
    userDescription :: String,
    userGroups :: [String]
}

instance Resource User where
    type Identifier User = String
    resourcePathSegment = "user"
    resourceIdentifier = userName

instance NamedResource User where
    resourceName = userName

instance DescribedResource User where
    resourceDescription = userDescription

instance ToJSON User where
    toJSON = toObject ["name" .=. userName, "user_owner" .=. userOwner, "fqhome" .=. userHome, "description" .=. userDescription, "groups" .=. userGroups]

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        userName <- o .: "name"
        userHome <- o .: "fqhome"
        userOwner <- o .: "user_owner"
        userDescription <- o .: "description"
        userGroups <- o .: "groups"
        return User{..}

listAllUsers :: MAS [User]
listAllUsers = listAllResource