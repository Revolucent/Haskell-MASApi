{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity (
    Entity(..),
    EntityResource(..),
    getEntityRaw,
    listEntityRaw
) 
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Ord
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
    resourcePathSegment = extraPathSegment @x

instance Eq (Entity x) where
    a == b = (entityName a) == (entityName b)

instance Ord (Entity x) where
    compare = comparing entityName

parseEntity :: forall x. (FromJSON x, Extra x) => Value -> Parser (Entity x)
parseEntity value = parseObject value
    where
        parseObject = let name = unpack (extraPathSegment @x) in
                        withObject name $ \o -> do
                            entityName <- o .: "name"
                            entityDescription <- o .: "description"
                            entityUserOwner <- o .: "user_owner"
                            entityDateCreated <- o .: "date_created"
                            entityDateUpdated <- o .: "date_updated"
                            entityPrivileges <- o .: "privileges"
                            entityClass <- o .: "entity"
                            entityExtra <- parseJSON value 
                            return Entity{..}

instance (FromJSON x, Extra x) => FromJSON (Entity x) where
    parseJSON = parseEntity 

listEntityRaw :: forall r e x. (Extra x, r ~ (Entity x), FromJSON e) => Bool -> MAS [e]
listEntityRaw unsummarized = listResourceRaw @r @e unsummarized 

getEntityRaw :: forall r e x. (Extra x, r ~ (Entity x), FromJSON e) => Identifier r -> MAS e
getEntityRaw = getResourceRaw @r @e