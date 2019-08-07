{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Constant (
    ConstantExtra(..),
    Constant,
    ConstantValueAttribute(..),
    createConstant,
    deleteConstant,
    getConstant,
    listConstants,
    modifyConstant
)

where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource
import Vendita.MAS.Strings

data ConstantExtra = ConstantExtra { constantValue :: String } deriving (Show)

instance Extra ConstantExtra where
    extraEntityClass = CONSTANT 

instance FromJSON ConstantExtra where
    parseJSON = withObject "constant" $ \o -> do
        constantValue <- o .: "value"
        return ConstantExtra{..}

type Constant = Entity ConstantExtra

data ConstantValueAttribute = ConstantValueAttribute

instance ToText ConstantValueAttribute where
    toText _ = "value"

instance AttributeKey ConstantValueAttribute where
    type AttributeValue ConstantValueAttribute = String
    type AttributeEntity ConstantValueAttribute = Constant

createConstant :: Identifier Constant -> String -> String -> MAS Constant 
createConstant name description value = createResource $ object [
        "name" .= name,
        "description" .= description,
        "value" .= value
    ] 

listConstants = listResource @Constant
getConstant = getResource @Constant
deleteConstant = deleteResource @Constant

modifyConstant :: Identifier Constant -> Attributes Constant -> MAS Constant
modifyConstant name attributes = modifyResource @Constant name $ object $ toPairs attributes