{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Invocations (
    Invocation (..),
    InvocationStatus (..),
    Parameters (..),
    (%=),
    ($=),
    invoke,
    invokeNow,
    listAllInvocations,
    listAllCurrentInvocations,
    listInvocations,
    withInvocationPeriod,
    getInvocation,
    listInvocationOutputs,
    getInvocationOutputText
) where

import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import qualified Data.HashMap.Strict as HMap
import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid
import Data.Time
import Data.UUID as UUID
import Data.UUID (UUID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Network.HTTP.Req ((=:))
import Text.Read (readMaybe)
import Vendita.MAS.Core

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS

data InvocationStatus = SCHEDULED | DELAYED | EXECUTING | SUCCEEDED | FAILED | UNKNOWN deriving (Eq, Ord, Enum, Read, Show)

instance FromJSON InvocationStatus where
    parseJSON (String string) = return $ fromMaybe UNKNOWN (readMaybe $ unpack string)
    parseJSON value = typeMismatch "Status" value

instance ToJSON InvocationStatus where
    toJSON = toJSON . show

newtype Parameters = Parameters (Map String Value) deriving (Eq, Read, Show, Semigroup, Monoid)

infixr 5 %=
infixr 5 $=

(%=) :: (ToJSON v) => String -> v -> Parameters
name %= value = Parameters $ Map.fromList [(name, toJSON value)]

($=) :: String -> String -> Parameters
($=) = (%=)

instance ToJSON Parameters where
    toJSON (Parameters parameters) = toJSON parameters 

instance FromJSON Parameters where
    parseJSON = fmap Parameters . parseJSON 

data VariadicParameter = VariadicParameter {
    variadicParameterName :: String,
    variadicParameterDescription :: String,
    variadicParameterType :: String,
    variadicParameterDeflt :: String
}

instance ToJSON VariadicParameter where
    toJSON VariadicParameter{..} = object [ 
            "name" .= variadicParameterName,
            "description" .= variadicParameterDescription,
            "type" .= variadicParameterType,
            "deflt" .= variadicParameterDeflt
        ]

data Invocation = Invocation {
    invocationUUID :: UUID,
    invocationProcess :: String,
    invocationDateInvoked :: MASTime, 
    invocationStatus :: InvocationStatus,
    invocationParameters :: Parameters,
    invocationErrors :: [String]
} deriving (Show)

instance FromJSON Invocation where
    parseJSON = withObject "Invocation" $ \o -> do
        invocationUUID <- o .: "uuid"
        invocationProcess <- o .: "process"
        invocationDateInvoked <- o .: "date_invoke"
        invocationStatus <- o .: "status"
        invocationParameters <- o .: "parameters"
        invocationErrors <- o .:? "errors" .!= []
        return Invocation{..}

instance ToJSON Invocation where
    toJSON Invocation{..} = object [ 
            "uuid" .= invocationUUID, 
            "process" .= invocationProcess,
            "date_invoke" .= invocationDateInvoked,
            "status" .= invocationStatus,
            "parameters" .= invocationParameters,
            "errors" .= invocationErrors
        ]

instance Resource Invocation where
    type Identifier Invocation = UUID
    resourceIdentifier = invocationUUID
    resourcePathSegment = "invocation"

withInvocationPeriod :: (MonadReader Connection m) => Day -> Integer -> m a -> m a
withInvocationPeriod dateInvoke period = withOption ("date_invoke" =: (formatTime defaultTimeLocale "%F" dateInvoke)) . withOption ("period" =: period)

listAllInvocations :: Day -> Integer -> MAS [Invocation]
listAllInvocations dateInvoke period = withInvocationPeriod dateInvoke period listAllResource 

listAllCurrentInvocations :: Integer -> MAS [Invocation]
listAllCurrentInvocations period = do
    start <- liftIO $ fmap (addDays (-period) . utctDay) getCurrentTime
    listAllInvocations start (period * 2)

listInvocations :: [Identifier Invocation] -> MAS [Invocation]
listInvocations = listResourceWithIdentifiers 

getInvocation :: (MonadFail f) => Identifier Invocation -> MAS (f Invocation)
getInvocation = firstResourceWithIdentifier

data InvocationOutputData = InvocationOutputData { invocationOutputDataText :: String } deriving (Show)

instance FromJSON InvocationOutputData where
    parseJSON = withObject "InvocationOutputData" $ \o -> do
        invocationOutputDataText <- o .: "text"
        return InvocationOutputData{..}

data InvocationOutput = InvocationOutput { invocationOutputData :: InvocationOutputData } deriving (Show)

instance FromJSON InvocationOutput where
    parseJSON = withObject "InvocationOutput" $ \o -> do
        invocationOutputData <- o .: "data"
        return InvocationOutput{..}

listInvocationOutputs :: Identifier Invocation -> MAS [InvocationOutput]
listInvocationOutputs uuid = list (withResource @Invocation $ withIdentifier uuid $ withPath "display" get)

getInvocationOutputText :: Identifier Invocation -> MAS String
getInvocationOutputText uuid = do
    lines <- (map (invocationOutputDataText.invocationOutputData)) <$> listInvocationOutputs uuid
    return $ intercalate "\n" lines

data NewInvocation = NewInvocation {
    newInvocationProcess :: String,
    newInvocationParameters :: Parameters,
    newInvocationTimestamp :: Maybe String 
}

instance ToJSON NewInvocation where
    toJSON = toObject [
            "process" .=. newInvocationProcess,
            "parameters" .=. newInvocationParameters,
            "timestamp" .=. newInvocationTimestamp
        ]

invoke :: String -> Parameters -> Maybe UTCTime -> MAS Invocation
invoke process parameters timestamp = do
    let invocation = NewInvocation {
            newInvocationProcess = process,
            newInvocationParameters = parameters,
            -- Not consistent! Why, Todd? WHY?
            newInvocationTimestamp = fmap (formatTime defaultTimeLocale "%F %T") timestamp
        }
    fmap envelopeFirst $ withEndpoint @Invocation $ post invocation

invokeNow :: String -> Parameters -> MAS Invocation
invokeNow process parameters = invoke process parameters Nothing