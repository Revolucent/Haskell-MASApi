module Vendita.MAS.Entity.Class (Class(..)) where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (pack)
import Vendita.MAS.Core
import Vendita.MAS.Resource

data Class = ACCOUNT | ALIAS | CONSTANT | EXCEPTION | FORM | NAMESPACE | PROCESS | PROTOTYPE | SCHEDULE | TYPE deriving (Eq, Ord, Enum, Read, Show)

instance EnumerationKey Class 

instance ToJSON Class where
    toJSON = enumerationToJSON

instance ToJSONKey Class where
    toJSONKey = enumerationToJSONKey
   
instance FromJSON Class where
    parseJSON = enumerationFromJSON

instance FromJSONKey Class where
    fromJSONKey = enumerationFromJSONKey

instance Pathed Class where
    pathSegment = pack . map toLower . show
