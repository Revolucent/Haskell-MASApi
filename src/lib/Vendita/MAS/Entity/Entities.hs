{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Entities (
    getEntity,
    listEntities
)

where

import Control.Applicative
import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Account
import Vendita.MAS.Entity.Alias
import Vendita.MAS.Entity.Constant
import Vendita.MAS.Entity.Exception
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Form
import Vendita.MAS.Entity.Generic
import Vendita.MAS.Entity.Process
import Vendita.MAS.Entity.Prototype
import Vendita.MAS.Entity.Schedule
import Vendita.MAS.Entity.Type
import Vendita.MAS.Resource

listEntities :: Bool -> MAS [Generic] 
listEntities unsummarized = foldr1 (liftA2 (++)) [
        listEntityRaw @Account unsummarized,
        listEntityRaw @Alias unsummarized,
        listEntityRaw @Constant unsummarized,
        listEntityRaw @Exception unsummarized,
        listEntityRaw @Form unsummarized,
        listEntityRaw @Process unsummarized,
        listEntityRaw @Prototype unsummarized,
        listEntityRaw @Schedule unsummarized,
        listEntityRaw @Type unsummarized
    ]

getEntity :: forall r x. (Extra x, r ~ (Entity x)) => Identifier r -> MAS Generic 
getEntity = getEntityRaw @r  