{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Attributes (
    AttributeKey(..),
    Attributes,
    attribute,
    rename,
    description,
    toPairs,
    (=:)
)
where

import Data.Aeson hiding ((.=))
import Data.Aeson.Types (Pair)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid (Endo)
import Vendita.MAS.Entity.Internal.Types
import Vendita.MAS.Strings (ToText(..))

class (ToText k, ToJSON (AttributeValue k)) => AttributeKey k where
    type AttributeValue k :: *
    type AttributeEntity k :: *

data AttributePair e = forall k v. (AttributeKey k, v ~ (AttributeValue k), e ~ (AttributeEntity k)) => AttributePair k v   

instance Eq (AttributePair e) where
    (AttributePair k1 _) == (AttributePair k2 _) = (toText k1) == (toText k2)

instance Ord (AttributePair e) where
    compare (AttributePair k1 _) (AttributePair k2 _) = compare (toText k1) (toText k2)

toPair :: AttributePair e -> Pair
toPair (AttributePair k v) = (toText k, toJSON v)

data Attributes e = Attributes (Set (AttributePair e))

toPairs :: Attributes e -> [Pair]
toPairs (Attributes set) = map toPair $ Set.toList set

instance Semigroup (Attributes e) where
    (Attributes set1) <> (Attributes set2) = Attributes $ set1 <> set2

instance Monoid (Attributes e) where
    mempty = Attributes mempty

infixr 8 `attribute`

attribute :: forall k v e. (AttributeKey k, v ~ (AttributeValue k), e ~ (AttributeEntity k)) => k -> v -> Attributes e
attribute k v = Attributes $ Set.singleton $ AttributePair k v

infixr 8 =: 

(=:) :: forall k v e. (AttributeKey k, v ~ (AttributeValue k), e ~ (AttributeEntity k)) => k -> v -> Attributes e
(=:) = attribute

data EntityAttributes e = Rename | Description

instance ToText (EntityAttributes e) where
    toText Rename = "rename"
    toText Description = "description"

instance EntityAttribute e => AttributeKey (EntityAttributes e) where
    type AttributeValue (EntityAttributes e) = String
    type AttributeEntity (EntityAttributes e) = e

rename :: forall e. EntityAttribute e => EntityAttributes e
rename = Rename

description :: forall e. EntityAttribute e => EntityAttributes e
description = Description
