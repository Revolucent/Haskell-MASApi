{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where 

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, FromJSON (..), (.:), (.:?), (.!=), withObject)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Req (https, (/:))
import System.Random (randomIO)
import Vendita.MAS as MAS
{- 
Yes, you need to provide Config.hs yourself. Should be blindingly obvious
what to do. Take a look at the first line of main below. 
-}
import Config 

examine :: MAS () 
examine = do
    (envelope :: Envelope Value) <- get
    let values = (envelopeContents envelope)
    liftIO $ forM_ values (LBS.putStr . encodePretty) 

data Alias = Alias { userOwner :: String }

data Prototype = Prototype { prototypeUUID :: UUID, prototypeName :: String, prototypeIsSystem :: Bool, prototypeIsCurrent :: Bool, prototypeVersion :: Int, prototypeDescription :: String, prototypeProperties :: [PrototypeProperty] } deriving (Show)

instance FromJSON Prototype where
    parseJSON = withObject "object" $ \obj -> do
        prototypeUUID <- obj .: "uuid"
        prototypeName <- obj .: "name"
        prototypeIsSystem <- obj .: "is_system"
        prototypeIsCurrent <- obj .: "is_current"
        prototypeVersion <- obj .: "version"
        prototypeDescription <- obj .: "description"
        prototypeProperties <- obj .:? "properties" .!= []
        return Prototype{..}

data PrototypeProperty = PrototypeProperty { prototypePropertyName :: String, prototypePropertyDescription :: String, prototypePropertyIsRequired :: Bool, prototypePropertyPosition :: Int } deriving (Show)

instance FromJSON PrototypeProperty where
    parseJSON = withObject "object" $ \obj -> do
        prototypePropertyName <- obj .: "name"
        prototypePropertyDescription <- obj .: "description"
        prototypePropertyIsRequired <- obj .: "is_required"
        prototypePropertyPosition <- obj .: "position"
        return PrototypeProperty{..}

instance Resource Prototype where
    type Identifier Prototype = UUID 
    resourceIdentifier = prototypeUUID

withPrototypeEndpoint :: (MonadReader Connection m) => m a -> m a
withPrototypeEndpoint = withPath "prototype"

withAccountEndpoint :: (MonadReader Connection m) => m a -> m a
withAccountEndpoint = withPath "credential"

withAliasEndpoint :: (MonadReader Connection m) => m a -> m a
withAliasEndpoint = withPath "alias"

listPrototypes :: [Identifier Prototype] -> MAS [Prototype] 
listPrototypes = withPrototypeEndpoint . listWithIdentifiers 

server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }

main :: IO ()
main = withServer server $ withAliasEndpoint examine 