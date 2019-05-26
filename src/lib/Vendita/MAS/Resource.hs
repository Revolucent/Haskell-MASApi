{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Vendita.MAS.Resource (
    Pathed(..),
    NamedAttribute(..),
    NamedResource(..),
    Resource(..),
    attributesToObject,
    createResource,
    deleteResource,
    getResource,
    listResource,
    listResourceRaw,
    modifyFQNamespace,
    modifyResource,
    modifyResourceAttributes,
    renameResource,
    withAll,
    withEndpoint,
    withIdentifier,
    withResource
) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.List.Split
import Data.Text hiding (last, map, splitOn)
import Data.UUID
import Network.HTTP.Req
import Vendita.MAS.Core

class Resource a where
    type Identifier a :: *
    resourceIdentifier :: a -> Identifier a
    resourceUserOwner :: a -> String
    resourceDateCreated :: a -> MASTime
    resourceDateUpdated :: a -> MASTime
    resourcePathSegment :: Text 
    resourceOptions :: Option 'Https
    resourceOptions = defaultPageSize <> defaultResponseTimeout

withEndpoint :: forall r a m. (Resource r, MonadReader Connection m) => m a -> m a
withEndpoint = withPath (resourcePathSegment @r)

withResource :: forall r a m. (Resource r, MonadReader Connection m) => m a -> m a
withResource = withEndpoint @r . withOption (resourceOptions @r)

class Pathed a where
    pathSegment :: a -> Text

instance Pathed String where
    pathSegment = pack

instance Pathed Text where
    pathSegment = id

instance Pathed UUID where
    pathSegment = pack . toString

class ToJSON a => NamedAttribute a where
    attributeName :: a -> Text 

attributesToObject :: (NamedAttribute a) => [a] -> Value
attributesToObject = object . map (\a -> (attributeName a) .= (toJSON a))

withIdentifier :: (MonadReader Connection m, Pathed i) => i -> m a -> m a
withIdentifier = withPath . pathSegment 

getResource :: forall r. (Resource r, FromJSON r, Pathed (Identifier r)) => Identifier r -> MAS r
getResource identifier = fmap envelopeFirst $ withResource @r $ withIdentifier identifier get

withAll :: (MonadReader Connection m) => Bool -> m a -> m a
withAll unsummarized m = if unsummarized then withPath "*" m else m

listResource :: forall r. (Resource r, FromJSON r) => Bool -> MAS [r]
listResource unsummarized = withResource @r $ withAll unsummarized $ list get

listResourceRaw :: forall r v. (Resource r, FromJSON v) => Bool -> MAS [v]
listResourceRaw unsummarized = withResource @r $ withAll unsummarized $ list get 

createResource :: forall r j. (Resource r, FromJSON r, ToJSON j) => j -> MAS r 
createResource definition = fmap envelopeFirst $ withEndpoint @r $ post definition 

modifyResource :: forall r j. (Resource r, FromJSON r, Pathed (Identifier r), ToJSON j) => Identifier r -> j -> MAS r
modifyResource name modification = fmap envelopeFirst $ withEndpoint @r $ withIdentifier name $ patch modification

modifyResourceAttributes :: forall r a. (Resource r, FromJSON r, Pathed (Identifier r), NamedAttribute a) => Identifier r -> [a] -> MAS r
modifyResourceAttributes name attributes = modifyResource name $ attributesToObject attributes

deleteResource :: forall r. (Resource r, Pathed (Identifier r)) => Identifier r -> MAS ()
deleteResource name = withEndpoint @r $ withIdentifier name delete_ 

class NamedResource r where
    resourceName :: r -> String

renameResource :: forall r. (Resource r, FromJSON r, Pathed (Identifier r), ToJSON (Identifier r)) => Identifier r -> Identifier r -> MAS r
renameResource old new = modifyResource @r old $ object [ "rename" .= new ]

modifyFQNamespace :: String -> String -> String
modifyFQNamespace fqname ns = let chunks = splitOn "." fqname in ns ++ "." ++ (last chunks)