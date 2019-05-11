{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vendita.MAS.Entity.Constant (
    ConstantExtra(..),
    Constant
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data ConstantExtra = ConstantExtra { constantValue :: String } deriving (Show)

instance Extra ConstantExtra where
    extraEntityClass = CONSTANT 

instance FromJSON ConstantExtra where
    parseJSON = withObject "constant" $ \o -> do 
        constantValue <- o .: "value"
        return ConstantExtra{..}

type Constant = Entity ConstantExtra
