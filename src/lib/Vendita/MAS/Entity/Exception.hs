{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Exception (
    ExceptionExtra(..),
    Exception,
    listExceptions
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data ExceptionExtra = ExceptionExtra { exceptionMessage :: String } deriving (Show)

instance Extra ExceptionExtra where
    extraEntityClass = EXCEPTION 

instance FromJSON ExceptionExtra where
    parseJSON = withObject "exception" $ \o -> do 
        exceptionMessage <- o .: "message"
        return ExceptionExtra{..}

type Exception = Entity ExceptionExtra

listExceptions = listResource @Exception