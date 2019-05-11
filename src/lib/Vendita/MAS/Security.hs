{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Security (
--    modifyEntityPrivilege,
--    modifyGroupEntityPrivilege,
--    modifyGroupOwner,
--    modifyOwner,
--    modifyUserEntityPrivilege,
--    modifyUserOwner
)
where

import Data.Aeson
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Resource
import Vendita.MAS.Security.Privilege
import Vendita.MAS.Security.Role

{-
modifyOwner :: forall e r. (Resource r, Role r, ToJSON (Identifier r), Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier r -> Identifier e -> MAS e
modifyOwner role name = withResource @e $ withPath "owner" $ withIdentifier name $ patch $ object [ "name" .= role, "is_group" .= roleIsGroup @r ]

modifyUserOwner :: forall e. (Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier User -> Identifier e -> MAS e
modifyUserOwner = modifyOwner @e @User

modifyGroupOwner :: forall e. (Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier Group -> Identifier e -> MAS e
modifyGroupOwner = modifyOwner @e @Group

modifyEntityPrivilege :: forall r e o. (Resource r, Role r, Pathed (Identifier r), Operator o, Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier r -> o -> EntityPrivilege -> Identifier e -> MAS e 
modifyEntityPrivilege role op privilege entity = withResource @r $ withIdentifier role $ withOperator op $ withEntityPrivilege privilege $ withIdentifier entity $ patch $ object [
        "cls" .= (entityResourceClass @e)
    ]

modifyUserEntityPrivilege :: forall e o. (Operator o, Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier User -> o -> EntityPrivilege -> Identifier e -> MAS e
modifyUserEntityPrivilege = modifyEntityPrivilege @User @e

modifyGroupEntityPrivilege :: forall e o. (Operator o, Resource e, EntityResource e, FromJSON e, Pathed (Identifier e)) => Identifier Group -> o -> EntityPrivilege -> Identifier e -> MAS e
modifyGroupEntityPrivilege = modifyEntityPrivilege @Group @e
-}