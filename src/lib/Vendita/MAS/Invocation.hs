{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Invocation (
    Invocation(..),
    InvocationParameters(..),
    InvocationStatus(..),
    getInvocation,
    invoke,
    invokeNow,
    listInvocations,
    listInvocationsWithRange,
    parameter,
    parameterS,
    withInvocationDateRange,
    withInvocationRange
)
where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text
import Data.Time
import Data.Time.Clock
import Data.UUID
import Network.HTTP.Req
import Text.Read (readMaybe)
import Vendita.MAS.Core
import Vendita.MAS.Entity.Process
import Vendita.MAS.Resource

data InvocationStatus = SUCCEEDED | SCHEDULED | EXECUTING | FAILED | DELAYED | KILLED | UNKNOWN deriving (Eq, Ord, Enum, Read, Show)

instance FromJSON InvocationStatus where
    parseJSON (String status) = case readMaybe (unpack status) of
        Just status -> return status
        _ -> return UNKNOWN
    parseJSON invalid = typeMismatch "Invalid type for InvocationStatus" invalid

data Invocation = Invocation {
    invocationUUID :: UUID,
    invocationProcess :: Identifier Process,
    invocationStatus :: InvocationStatus,
    invocationParameters :: Object,
    invocationUserOwner :: String,
    invocationDateCreated :: MASTime,
    invocationDateUpdated :: MASTime
} deriving (Show)

instance Resource Invocation where
    type Identifier Invocation = UUID
    resourceIdentifier = invocationUUID
    resourcePathSegment = "invocation"

instance FromJSON Invocation where
    parseJSON = withObject "invocation" $ \o -> do
        invocationUUID <- o .: "uuid"
        invocationProcess <- o .: "process"
        invocationStatus <- o .: "status"
        invocationParameters <- o .: "parameters"
        invocationUserOwner <- o .: "user_owner"
        invocationDateCreated <- o .: "date_created"
        invocationDateUpdated <- o .: "date_updated"
        return Invocation{..}

withInvocationDateRange :: (MonadReader Connection m) => Day -> Integer -> m a -> m a
withInvocationDateRange day period m = do
    let start = formatTime defaultTimeLocale "%F" day
    withOption ("date_invoke" =: start) $ withOption ("period" =: period) m

withInvocationRange :: (MonadReader Connection m, MonadIO m) => Integer -> Integer -> m a -> m a
withInvocationRange back forth m = do
    start <- liftIO $ fmap (addDays (-back) . utctDay) getCurrentTime
    withInvocationDateRange start (back + forth) m

data InvocationParameters = InvocationParameters (Map String Value) deriving (Show)

instance Semigroup InvocationParameters where
    (InvocationParameters a) <> (InvocationParameters b) = InvocationParameters (a <> b)

instance Monoid InvocationParameters where
    mappend (InvocationParameters a) (InvocationParameters b) = InvocationParameters (mappend a b) 
    mempty = InvocationParameters mempty

instance ToJSON InvocationParameters where
    toJSON (InvocationParameters parameters) = toJSON parameters

parameter :: (ToJSON v) => String -> v -> InvocationParameters
parameter name value = InvocationParameters (Map.fromList [(name, toJSON value)])

parameterS :: String -> String -> InvocationParameters
parameterS = parameter

getInvocation = getResource @Invocation

listInvocations :: Day -> Integer -> Bool -> MAS [Invocation]
listInvocations start period unsummarized = withInvocationDateRange start period $ listResource unsummarized

listInvocationsWithRange :: Integer -> Integer -> Bool -> MAS [Invocation]
listInvocationsWithRange back forth unsummarized = withInvocationRange back forth $ listResource unsummarized

invoke :: String -> InvocationParameters -> Maybe UTCTime -> MAS Invocation
invoke process parameters timestamp = fmap envelopeFirst $ withResource @Invocation $ post $ object $ filterNulls [
        "process" .= process,
        "parameters" .= parameters,
        "timestamp" .= timestamp
    ]

invokeNow process parameters = invoke process parameters Nothing