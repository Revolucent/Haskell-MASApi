{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Vendita.MAS.Resource (
    Pathed(..),
    Resource(..),
    createResource,
    deleteResource,
    getResource,
    getResourceRaw,
    listResource,
    listResourceRaw,
    modifyFQNamespace,
    modifyResource,
    withAll,
    withEndpoint,
    withIdentifier,
    withResource
) where

import Control.Monad.Reader
import Data.Ord
import Data.Aeson hiding ((.=))
import Data.List.Split
import Data.Text hiding (last, map, splitOn)
import Data.UUID hiding (toText)
import Network.HTTP.Req
import Vendita.MAS.Core
import Vendita.MAS.Strings hiding (toString)

class Resource a where
    type Identifier a :: *
    resourceIdentifier :: a -> Identifier a
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

getResourceRaw :: forall r v. (Resource r, FromJSON v, Pathed (Identifier r)) => Identifier r -> MAS v
getResourceRaw identifier = fmap envelopeFirst $ withResource @r $ withIdentifier identifier $ get

getResource :: forall r. (Resource r, FromJSON r, Pathed (Identifier r)) => Identifier r -> MAS r
getResource = getResourceRaw @r

withAll :: (MonadReader Connection m) => Bool -> m a -> m a
withAll unsummarized m = if unsummarized then withPath "*" m else m

listResourceRaw :: forall r v. (Resource r, FromJSON v) => Bool -> MAS [v]
listResourceRaw unsummarized = withResource @r $ withAll unsummarized $ list get 

listResource :: forall r. (Resource r, FromJSON r) => Bool -> MAS [r]
-- listResource unsummarized = withResource @r $ withAll unsummarized $ list get
listResource = listResourceRaw @r

createResourceRaw :: forall r v j. (Resource r, FromJSON v, ToJSON j) => j -> MAS v
createResourceRaw definition = envelopeFirst <$> withEndpoint @r (post definition)

createResource :: forall r j. (Resource r, FromJSON r, ToJSON j) => j -> MAS r 
createResource = createResourceRaw @r

modifyResourceRaw :: forall r v j. (Resource r, FromJSON v, Pathed (Identifier r), ToJSON j) => Identifier r -> j -> MAS v
modifyResourceRaw name modification = envelopeFirst <$> withEndpoint @r (withIdentifier name $ patch modification)

modifyResource :: forall r j. (Resource r, FromJSON r, Pathed (Identifier r), ToJSON j) => Identifier r -> j -> MAS r
modifyResource = modifyResourceRaw @r 

deleteResource :: forall r. (Resource r, Pathed (Identifier r)) => Identifier r -> MAS ()
deleteResource name = withEndpoint @r $ withIdentifier name delete_ 

modifyFQNamespace :: String -> String -> String
modifyFQNamespace fqname ns = let chunks = splitOn "." fqname in ns ++ "." ++ (last chunks)