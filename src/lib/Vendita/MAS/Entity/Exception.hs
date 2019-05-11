{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Exception (
    ExceptionExtra(..),
    Exception
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data ExceptionExtra = ExceptionExtra { exceptionMessage :: String } deriving (Show)

instance Extra ExceptionExtra where
    extraEntityClass = EXCEPTION 

instance FromJSON ExceptionExtra where
    parseJSON = withObject "exception" $ \o -> do 
        exceptionMessage <- o .: "message"
        return ExceptionExtra{..}

type Exception = Entity ExceptionExtra
