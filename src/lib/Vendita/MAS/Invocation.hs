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
    defaultPollingStrategy,
    getInvocation,
    getInvocationOutputs,
    getInvocationOutputText,
    invoke,
    invokeNow,
    isInvocationComplete,
    isInvocationIncomplete,
    listInvocations,
    listInvocationsWithRange,
    makePollingStrategy,
    noPollingCallback,
    parameter,
    parameterS,
    poll,
    pollDefault,
    withInvocationDateRange,
    withInvocationRange,
    withInvocationTimeRange,
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

withInvocationTimeRange :: (MonadReader Connection m) => UTCTime -> Integer -> m a -> m a
withInvocationTimeRange time period m = do
    let start = formatTime defaultTimeLocale "%FT%T" time
    withOption ("date_invoke" =: start) $ withOption ("period" =: period) m

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

getInvocationOutputs :: Identifier Invocation -> MAS [InvocationOutput]
getInvocationOutputs uuid = withEndpoint @Invocation $ withIdentifier uuid $ withPath "display" $ list get

getInvocationOutputText :: Identifier Invocation -> MAS String
getInvocationOutputText uuid = concat . map invocationOutputText <$> getInvocationOutputs uuid 

makePollingStrategy :: NominalDiffTime -> NominalDiffTime -> Invocation -> UTCTime -> Maybe NominalDiffTime
makePollingStrategy defaultInterval defaultTimeout invocation now = do
    let diff = now `diffUTCTime` (utcTime $ invocationDateInvoked invocation)
    if diff >= defaultTimeout
        then Nothing -- Abort!
        else if isInvocationComplete invocation
            then Nothing 
            else Just defaultInterval

defaultPollingStrategy = makePollingStrategy 2.5 240

noPollingCallback :: Invocation -> MAS ()
noPollingCallback _ = return () 

poll :: (Invocation -> UTCTime -> Maybe NominalDiffTime) -> (Invocation -> MAS ()) -> Invocation -> MAS Invocation
poll pollingStrategy callback invocation = do
    callback invocation
    now <- liftIO getCurrentTime
    let maybeInterval = pollingStrategy invocation now
    case maybeInterval of
        Nothing -> return invocation
        Just interval -> do 
            liftIO $ do
                let seconds = fromRational $ toRational interval
                threadDelay $ round $ (1000000 * seconds)
            getInvocation (invocationUUID invocation) >>= poll pollingStrategy callback

pollDefault = poll defaultPollingStrategy noPollingCallback