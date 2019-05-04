{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Processes (
    Expression (..),
    NewProcess(..),
    Process (..),
    ProcessParameter(..),
    Step (..),
    deleteProcess,
    emptyNewProcess,
    getProcess,
    listAllProcesses,
    listProcesses,
    patchProcess
) where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entities.Privileges

data ProcessParameter = ProcessParameter {
    processParameterName :: String,
    processParameterDescription :: String,
    processParameterDataType :: String,
    processParameterDefault :: Maybe String,
    processParameterMeta :: Object
} deriving (Show)

instance ToJSON ProcessParameter where
    toJSON = toObject [
            "name" .=. processParameterName,
            "description" .=. processParameterDescription,
            "data_type" .=. processParameterDataType,
            "deflt" .=. processParameterDefault,
            "meta" .=. processParameterMeta
        ]

instance FromJSON ProcessParameter where
    parseJSON = withObject "Parameter" $ \o -> do
        processParameterName <- o .: "name"
        processParameterDescription <- o .: "description"
        processParameterDataType <- o .: "data_type"
        processParameterDefault <- o .: "deflt"
        processParameterMeta <- o .: "meta"
        return ProcessParameter{..}

data Process = Process {
    processName :: String,
    processDescription :: String,
    processParameters :: [ProcessParameter],
    processSteps :: Maybe [Step],
    processPrivileges :: Maybe Privileges
} deriving (Show)

instance FromJSON Process where
    parseJSON = withObject "process" $ \o -> do
        processName <- o .: "name"
        processDescription <- o .: "description"
        processParameters <- o .: "parameters"
        processSteps <- o .: "steps"
        processPrivileges <- o .: "privileges"
        return Process{..}

instance ToJSON Process where
    toJSON = toObject [
            "name" .=. processName,
            "description" .=. processDescription,
            "parameters" .=. processParameters,
            "steps" .=. processSteps,
            "privileges" .=. processPrivileges
        ]

data NewProcess = NewProcess {
    newProcessRename :: Maybe String,
    newProcessDescription :: Maybe String,
    newProcessParameters :: Maybe [ProcessParameter],
    newProcessSteps :: Maybe [Step]
} deriving (Show)

instance ToJSON NewProcess where
    toJSON NewProcess{..} = object $ filterNulls [ 
            "rename" .= newProcessRename, 
            "description" .= newProcessDescription,
            "parameters" .= newProcessParameters, 
            "steps" .= newProcessSteps 
        ]

emptyNewProcess = NewProcess { newProcessRename = Nothing, newProcessDescription = Nothing, newProcessParameters = Nothing, newProcessSteps = Nothing }

instance Resource Process where
    type Identifier Process = String
    resourceIdentifier = processName
    resourcePathSegment = "process"

instance NamedResource Process where
    resourceName = processName

instance DescribedResource Process where
    resourceDescription = processDescription
    
data Step = Step {
    stepStep :: String,
    stepLabel :: String,
    stepDescription :: Maybe String,
    stepExpressions :: [Expression],
    stepTransitions :: [Object]
} deriving (Show)

instance FromJSON Step where
    parseJSON = withObject "process step" $ \o -> do
        stepStep <- o .: "step"
        stepLabel <- o .: "label"
        stepDescription <- o .: "description"
        stepExpressions <- o .: "expressions"
        stepTransitions <- o .: "transitions"
        return Step{..}

instance ToJSON Step where
    toJSON Step{..} = object [
            "step" .= stepStep,
            "label" .= stepLabel, 
            "description" .= stepDescription,
            "expressions" .= stepExpressions,
            "transitions" .= stepTransitions,
            "x" .= (0.0 :: Double),
            "y" .= (0.0 :: Double)
        ]

data Expression = Expression {
    expressionAssign :: Maybe String,
    expressionWeight :: Int,
    expressionEvaluate :: Maybe String,
    expressionConnection :: Maybe String,
    expressionParameters :: Maybe Object 
} deriving (Show)

instance FromJSON Expression where
    parseJSON = withObject "process expression" $ \o -> do
        expressionAssign <- o .:? "assign"
        expressionWeight <- o .: "weight"
        expressionEvaluate <- o .:? "evaluate"
        expressionConnection <- o .:? "connection"
        expressionParameters <- o .:? "parameters"
        return Expression{..}

instance ToJSON Expression where
    toJSON = toObject [
            "assign" .=. expressionAssign,
            "weight" .=. expressionWeight,
            "evaluate" .=. expressionEvaluate,
            "connection" .=. expressionConnection,
            "parameters" .=. expressionParameters
        ]

listAllProcesses :: MAS [Process]
listAllProcesses = listAllResource

listProcesses :: MAS [Process]
listProcesses = listResource

getProcess :: Identifier Process -> MAS (Maybe Process)
getProcess = firstResourceWithIdentifier

deleteProcess :: Identifier Process -> MAS ()
deleteProcess = deleteResourceWithIdentifier_ @Process 

patchProcess :: Identifier Process -> NewProcess -> MAS Process
patchProcess name process = fmap envelopeFirst $ withResource @Process $ withIdentifier name $ patch process