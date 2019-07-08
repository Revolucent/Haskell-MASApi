{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Invocation (
    Invocation(..),
    InvocationParameters,
    InvocationStatus(..),
    abortPollingAfter,
    getInvocation,
    getInvocationOutputs,
    getInvocationOutputText,
    invoke,
    invokeNow,
    isInvocationComplete,
    isInvocationIncomplete,
    listInvocations,
    listInvocationsWithRange,
    parameter,
    parameterS,
    poll,
    pollNow,
    withInvocationDateRange,
    withInvocationRange,
    (=#),
    (=$),
    (=$$)
)
where

import Control.Concurrent
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Ord
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text hiding (concat, map)
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
    invocationDateInvoked :: MASTime,
    invocationStatus :: InvocationStatus,
    invocationErrors :: [String],
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
        invocationDateInvoked <- o .: "date_invoke"
        invocationStatus <- o .: "status"
        invocationErrors <- fromMaybe [] <$> o .:? "errors"
        invocationParameters <- o .: "parameters"
        invocationUserOwner <- o .: "user_owner"
        invocationDateCreated <- o .: "date_created"
        invocationDateUpdated <- o .: "date_updated"
        return Invocation{..}

instance Eq Invocation where
    a == b = (invocationUUID a) == (invocationUUID b)

instance Ord Invocation where
    compare = comparing invocationUUID

isInvocationIncomplete :: Invocation -> Bool
isInvocationIncomplete invocation = (invocationStatus invocation) `elem` [DELAYED, EXECUTING, SCHEDULED]

isInvocationComplete :: Invocation -> Bool
isInvocationComplete = not . isInvocationIncomplete 

data InvocationOutput = InvocationOutput {
    invocationOutputText :: String
}  deriving (Show)

instance FromJSON InvocationOutput where
    parseJSON = withObject "output" $ \o -> do
        info <- o .: "data"
        invocationOutputText <- info .: "text"
        return InvocationOutput{..}

withInvocationDateRange :: (MonadReader Connection m) => Day -> Integer -> m a -> m a
withInvocationDateRange day period m = do
    let start = formatTime defaultTimeLocale "%F" day
    withOption ("date_invoke" =: start) $ withOption ("period" =: period) m

withInvocationRange :: (MonadReader Connection m, MonadIO m) => Integer -> Integer -> m a -> m a
withInvocationRange back forth m = do
    start <- liftIO $ fmap (addDays (-back) . utctDay) getCurrentTime
    withInvocationDateRange start (back + forth) m

type InvocationParameters = Map String Value

infixr 8 =# 

(=#) :: (ToJSON v) => String -> v -> InvocationParameters
name =# value = Map.singleton name $ toJSON value

parameter :: (ToJSON v) => String -> v -> InvocationParameters
parameter name value = Map.singleton name $ toJSON value 

infixr 8 =$
infixr 8 =$$

(=$) :: String -> String -> InvocationParameters 
(=$) = (=#)

(=$$) :: String -> [String] -> InvocationParameters
(=$$) = (=#)

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

abortPollingAfter :: Integer -> UTCTime -> Invocation -> MAS Bool
abortPollingAfter seconds date = \_ -> do
    now <- liftIO getCurrentTime
    let diff = now `diffUTCTime` date
    return $ diff < (realToFrac seconds)

poll :: Identifier Process -> InvocationParameters -> Maybe UTCTime -> Maybe (UTCTime -> Invocation -> MAS Bool) -> MAS Invocation 
poll name parameters maybeWhen maybeCallback = do
    start <- liftIO getCurrentTime
    invoke name parameters maybeWhen >>= poll' start
    where
        poll' :: UTCTime -> Invocation -> MAS Invocation 
        poll' start invocation
            | isInvocationIncomplete invocation = do
                continue <- callback start invocation
                if continue 
                    then do
                        liftIO $ threadDelay 2500000 -- Pause for 2.5 seconds
                        getInvocation (invocationUUID invocation) >>= poll' start
                    else return invocation
            | otherwise = return invocation
        callback = fromMaybe (\_ _ -> return True) maybeCallback

pollNow :: Identifier Process -> InvocationParameters -> Maybe (UTCTime -> Invocation -> MAS Bool) -> MAS Invocation
pollNow name parameters maybeCallback = poll name parameters Nothing maybeCallback

getInvocationOutputs :: Identifier Invocation -> MAS [InvocationOutput]
getInvocationOutputs uuid = withEndpoint @Invocation $ withIdentifier uuid $ withPath "display" $ list get

getInvocationOutputText :: Identifier Invocation -> MAS String
getInvocationOutputText uuid = concat . map invocationOutputText <$> getInvocationOutputs uuid 
