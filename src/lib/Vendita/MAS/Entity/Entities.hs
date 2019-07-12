{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Set (Set)
import Data.Semigroup 
import Data.Monoid
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Account
import Vendita.MAS.Entity.Alias
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Constant
import Vendita.MAS.Entity.Exception
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Form
import Vendita.MAS.Entity.Generic
import Vendita.MAS.Entity.Process
import Vendita.MAS.Entity.Prototype
import Vendita.MAS.Entity.Type
import Vendita.MAS.Resource

data EntityNode = EntityNode String Class | NamespaceNode String EntityTree | AliasNode String String deriving (Show)

entityNodeName :: EntityNode -> String
entityNodeName (EntityNode name _) = name
entityNodeName (NamespaceNode name _) = name
entityNodeName (AliasNode name _) = name

instance Eq EntityNode where
    a == b = (entityNodeName a) == (entityNodeName b)

instance Ord EntityNode where
    compare a b = compare (entityNodeName a) (entityNodeName b)

data EntityTree = EntityTree (Set EntityNode) deriving (Show)

instance Semigroup EntityTree where
    (EntityTree trees1) <> (EntityTree trees2) = EntityTree $ trees1 <> trees2

instance Monoid EntityTree where
    mempty = EntityTree mempty 

listEntities :: Bool -> MAS [Generic] 
listEntities unsummarized = foldr1 (liftA2 (++)) [
        listEntityRaw @Account unsummarized,
        listEntityRaw @Alias unsummarized,
        listEntityRaw @Constant unsummarized,
        listEntityRaw @Exception unsummarized,
        listEntityRaw @Form unsummarized,
        listEntityRaw @Process unsummarized,
        listEntityRaw @Prototype unsummarized,
        listEntityRaw @Type unsummarized
    ]

getEntity :: forall r x. (Extra x, r ~ (Entity x)) => Identifier r -> MAS Generic 
getEntity = getEntityRaw @r  