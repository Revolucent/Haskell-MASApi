{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Processes (
    Expression (..),
    Process (..),
    Step (..),
    listAllProcesses,
    getProcess
) where

import Data.Aeson
import Vendita.MAS.Core

data Process = Process {
    processName :: String,
    processDescription :: String,
    processSteps :: [Step]
} deriving (Show)

instance FromJSON Process where
    parseJSON = withObject "process" $ \o -> do
        processName <- o .: "name"
        processDescription <- o .: "description"
        processSteps <- o .: "steps"
        return Process{..}

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
    stepExpressions :: [Expression],
    stepTransitions :: [Object]
} deriving (Show)

instance FromJSON Step where
    parseJSON = withObject "process step" $ \o -> do
        stepStep <- o .: "step"
        stepExpressions <- o .: "expressions"
        stepTransitions <- o .: "transitions"
        return Step{..}

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

listAllProcesses :: MAS [Process]
listAllProcesses = listAllResource

getProcess :: Identifier Process -> MAS (Maybe Process)
getProcess = firstResourceWithIdentifier