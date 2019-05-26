{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Process (
    Process,
    ProcessExtra(..),
    ProcessParameter(..),
    ProcessParameterMeta(..),
    getProcess,
    listProcesses
)

where

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Set
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Type
import Vendita.MAS.Resource

data ProcessParameterMeta = ProcessParameterMeta {
    processParameterMetaEditor :: Maybe String
} deriving (Show)

instance FromJSON ProcessParameterMeta where
    parseJSON = withObject "meta" $ \o -> do
        temp <- parseJSON $ toJSON o
        maybeEditor <- temp .:? "editor"
        processParameterMetaEditor <- case maybeEditor of
            Just editor -> parseJSON editor 
            _ -> return Nothing
        return ProcessParameterMeta{..}

data ProcessParameter = ProcessParameter {
    processParameterName :: String,
    processParameterDescription :: String,
    processParameterType :: Identifier Type,
    processParameterDefault :: Maybe String,
    processParameterMeta :: ProcessParameterMeta 
} deriving (Show)

instance Eq ProcessParameter where
    a == b = (processParameterName a) == (processParameterName b)

instance Ord ProcessParameter where
    compare a b = compare (processParameterName a) (processParameterName b)

instance FromJSON ProcessParameter where
    parseJSON = withObject "parameter" $ \o -> do
        processParameterName <- o .: "name"
        processParameterDescription <- o .: "description"
        processParameterType <- o .: "data_type"
        processParameterDefault <- o .: "deflt"
        processParameterMeta <- o .: "meta"
        return ProcessParameter{..}

data ProcessExtra = ProcessExtra { 
    processParameters :: Set ProcessParameter 
} deriving (Show)

instance Extra ProcessExtra where
    extraEntityClass = PROCESS 

instance FromJSON ProcessExtra where
    parseJSON = withObject "process" $ \o -> do
        processParameters <- o .: "parameters" 
        return ProcessExtra{..}

type Process = Entity ProcessExtra

getProcess = getResource @Process
listProcesses = listResource @Process