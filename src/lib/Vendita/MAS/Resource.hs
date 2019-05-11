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
    NamedResource(..),
    Resource(..),
    getResource,
    listResource,
    listResourceRaw,
    withAll,
    withEndpoint,
    withIdentifier,
    withResource
) where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
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

class NamedResource r where
    resourceName :: r -> String

renameResource :: forall r. (NamedResource r, Resource r, FromJSON r, Pathed (Identifier r), ToJSON (Identifier r)) => Identifier r -> Identifier r -> MAS r
renameResource old new = fmap envelopeFirst $ withResource @r $ withIdentifier old $ patch $ object [ "rename" .= new ]