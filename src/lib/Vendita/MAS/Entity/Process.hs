{-# LANGUAGE OverloadedStrings #-}

module Vendita.MAS.Entity.Process (
    ProcessExtra(..),
    Process
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data ProcessExtra = ProcessExtra deriving (Show)

instance Extra ProcessExtra where
    extraEntityClass = PROCESS 

instance FromJSON ProcessExtra where
    parseJSON _ = return ProcessExtra

type Process = Entity ProcessExtra
