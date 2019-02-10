{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Invocations (
    Invocation (..),
    listAllInvocations,
    listInvocations,
    withInvocationDateRange
) where

import Control.Monad.Reader
import Data.Aeson
import Data.UUID as UUID
import Data.UUID (UUID)
import Data.Map (Map)
import Network.HTTP.Req ((=:))
import Vendita.MAS.Core

data Invocation = Invocation {
    invocationUUID :: UUID,
    invocationDateInvoked :: String, -- for now
    invocationParameters :: Map String Value 
} deriving (Show)

instance FromJSON Invocation where
    parseJSON = withObject "invocation" $ \o -> do
        invocationUUID <- o .: "uuid"
        invocationDateInvoked <- o .: "date_invoke"
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

