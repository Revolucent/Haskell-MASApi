{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Users (
    User(..),
    listAllUsers,
    createUser,
    deleteUsers,
    deleteUser
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
    userGroups :: [String]
}

instance Resource User where
    type Identifier User = String
    resourcePathSegment = "user"
    resourceIdentifier = userName

instance NamedResource User where
    resourceName = userName

instance ToJSON User where
    toJSON = toObject ["name" .=. userName, "user_owner" .=. userOwner, "fqhome" .=. userHome, "groups" .=. userGroups]

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        userName <- o .: "name"
        userHome <- o .: "fqhome"
        userOwner <- o .: "user_owner"
        userGroups <- o .: "groups"
        return User{..}

data NewUser = NewUser {
    newUserName :: String,
    newUserPassword :: String,
    newUserDescription :: String,
    newUserGroups :: [String]
} deriving (Eq, Show)

instance ToJSON NewUser where
    toJSON = toObject [ 
                "name" .=. newUserName, 
                "password" .=. newUserPassword, 
                "description" .=. newUserDescription, 
                "groups" .=. newUserGroups
            ]

listAllUsers :: MAS [User]
listAllUsers = listAllResource

createUser :: String -> String -> String -> [String] -> MAS User 
createUser name password description groups = fmap envelopeFirst $ withEndpoint @User $ post NewUser { newUserName = name, newUserPassword = password, newUserDescription = description, newUserGroups = groups }

deleteUsers :: [Identifier User] -> MAS ()
deleteUsers = deleteResourceWithIdentifiers_ @User

deleteUser :: Identifier User -> MAS ()
deleteUser name = deleteUsers [name]
