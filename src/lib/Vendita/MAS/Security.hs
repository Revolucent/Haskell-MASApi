{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Security (
    modifyClassPrivilege,
    modifyEntityPrivilege,
    modifyEntityOwner
)
where

import Data.Aeson
import Network.HTTP.Req (NoReqBody(..), PATCH(..))
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Resource
import Vendita.MAS.Security.Privilege
import Vendita.MAS.Security.Role

modifyEntityOwner :: forall e r. (Resource e, EntityResource e, Pathed (Identifier e), FromJSON e, Resource r, Role r, ToJSON (Identifier r)) => Identifier e -> Identifier r -> MAS e 
modifyEntityOwner name role = fmap envelopeFirst $ withEndpoint @e $ withPath "owner" $ withIdentifier name $ patch $ object [
        "role" .= role,
        "is_group" .= roleIsGroup @r
    ]

modifyEntityPrivilege :: forall r e o. (Resource r, Role r, Pathed (Identifier r), Operator o, Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier r -> o -> EntityPrivilege -> Identifier e -> MAS e 
modifyEntityPrivilege role op priv name = fmap envelopeFirst $ withEndpoint @r $ withIdentifier role $ withOperator op $ withEntityPrivilege priv $ withIdentifier name $ patch $ object [
        "cls" .= resourcePathSegment @e
    ]

modifyClassPrivilege :: forall r o. (Resource r, Role r, Pathed (Identifier r), FromJSON r, Operator o) => Identifier r -> o -> ClassPrivilege -> Class -> MAS r
modifyClassPrivilege role op priv c = fmap envelopeFirst $ withEndpoint @r $ withIdentifier role $ withOperator op $ withClassPrivilege priv $ withPath (pathSegment c) $ mas PATCH NoReqBody