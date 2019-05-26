{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Constant (
    ConstantExtra(..),
    Constant,
    createConstant,
    deleteConstant,
    getConstant,
    listConstants
)

where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data ConstantExtra = ConstantExtra { constantValue :: String } deriving (Show)

instance Extra ConstantExtra where
    extraEntityClass = CONSTANT 

instance FromJSON ConstantExtra where
    parseJSON = withObject "constant" $ \o -> do
        constantValue <- o .: "value"
        return ConstantExtra{..}

type Constant = Entity ConstantExtra

createConstant :: Identifier Constant -> String -> String -> MAS Constant 
createConstant name description value = createResource $ object [
        "name" .= name,
        "description" .= description,
        "value" .= value
    ] 

listConstants = listResource @Constant
getConstant = getResource @Constant
deleteConstant = deleteResource @Constant