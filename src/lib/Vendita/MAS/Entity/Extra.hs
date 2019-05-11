{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Extra (
    Extra(..)
)
where

import Data.Text
import Vendita.MAS.Entity.Class
import Vendita.MAS.Resource

class Extra a where
    extraEntityClass :: Class
    extraPathSegment :: Text
    extraPathSegment = pathSegment $ extraEntityClass @a 


    