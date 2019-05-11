{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity (
    Entity(..)
) 
where

import Data.Aeson
import Data.Text (unpack)
import Vendita.MAS.Core
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource
import Vendita.MAS.Security.Privilege

class EntityResource e where
    entityResourceClass :: Class

data Entity x = Entity {
    entityName :: String,
    entityDescription :: String,
    entityUserOwner :: String,
    entityDateCreated :: MASTime,
    entityDateUpdated :: MASTime,
    entityPrivileges :: Maybe EntityPrivileges,
    entityClass :: Class,
    entityExtra :: x
} deriving (Show)

instance (Extra x) => EntityResource (Entity x) where
    entityResourceClass = extraEntityClass @x

instance NamedResource (Entity x) where
    resourceName = entityName

instance (Extra x) => Resource (Entity x) where
    type Identifier (Entity x) = String
    resourceIdentifier = entityName
    resourceUserOwner = entityUserOwner
    resourceDateCreated = entityDateCreated
    resourceDateUpdated = entityDateUpdated
    resourcePathSegment = extraPathSegment @x

instance (FromJSON x, Extra x) => FromJSON (Entity x) where
    parseJSON = let name = unpack (extraPathSegment @x) in
                    withObject name $ \o -> do
                        entityName <- o .: "name"
                        entityDescription <- o .: "description"
                        entityUserOwner <- o .: "user_owner"
                        entityDateCreated <- o .: "date_created"
                        entityDateUpdated <- o .: "date_updated"
                        entityPrivileges <- o .: "privileges"
                        entityClass <- o .: "entity"
                        entityExtra <- parseJSON $ toJSON o 
                        return Entity{..}