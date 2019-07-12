module Vendita.MAS.Strings (FromString(..), ToString(..), FromText(..), ToText(..)) where

import Data.Text (Text, pack)
import Text.Read (readMaybe)

class FromString a where
    fromString :: String -> Maybe a

class ToString a where
    toString :: a -> String

class FromText a where
    fromText :: Text -> Maybe a

class ToText a where
    toText :: a -> Text