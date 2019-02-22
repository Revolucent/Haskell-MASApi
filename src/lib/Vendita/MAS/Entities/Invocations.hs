{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Invocations (
    Invocation (..),
    InvocationStatus (..),
    listAllInvocations,
    listInvocations,
    withInvocationDateRange,
    getInvocation,
    listInvocationOutputs,
    getInvocationOutputText
) where

import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.UUID as UUID
import Data.UUID (UUID)
import Data.Map (Map)
import Data.Text (unpack)
import Network.HTTP.Req ((=:))
import Text.Read (readMaybe)
import Vendita.MAS.Core

data InvocationStatus = SCHEDULED | DELAYED | EXECUTING | SUCCEEDED | FAILED | UNKNOWN deriving (Eq, Ord, Enum, Read, Show)

instance FromJSON InvocationStatus where
    parseJSON (String string) = return $ fromMaybe UNKNOWN (readMaybe $ unpack string)
    parseJSON value = typeMismatch "Status" value

data Invocation = Invocation {
    invocationUUID :: UUID,
    invocationProcess :: String,
    invocationDateInvoked :: String, -- for now
    invocationStatus :: InvocationStatus,
    invocationParameters :: Map String Value 
} deriving (Show)

instance FromJSON Invocation where
    parseJSON = withObject "Invocation" $ \o -> do
        invocationUUID <- o .: "uuid"
        invocationProcess <- o .: "process"
        invocationDateInvoked <- o .: "date_invoke"
        invocationStatus <- o .: "status"
        invocationParameters <- o .: "parameters"
        return Invocation{..}

instance Resource Invocation where
    type Identifier Invocation = UUID
    resourceIdentifier = invocationUUID
    resourcePathSegment = "invocation"

withInvocationDateRange :: (MonadReader Connection m) => String -> Int -> m a -> m a
withInvocationDateRange dateInvoke period = withOption ("date_invoke" =: dateInvoke) . withOption ("period" =: period)

listAllInvocations :: MAS [Invocation]
listAllInvocations = listAllResource 

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