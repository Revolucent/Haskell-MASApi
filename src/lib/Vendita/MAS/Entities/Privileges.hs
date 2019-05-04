{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Privileges (
    ActualClassPrivileges (..),
    ActualPrivileges (..),
    ClassPrivileges (..),
    Permission (..),
    Privileges (..)
) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Vendita.MAS.Core

data Permission = READ | WRITE | EXECUTE | AFFILIATE | CREATE deriving (Eq, Ord, Enum)

instance Show Permission where
    show READ = "read"
    show WRITE = "write"
    show EXECUTE = "execute"
    show AFFILIATE = "affiliate"
    show CREATE = "create"

instance Read Permission where
    readsPrec _ value = tryParse [("read", READ), ("write", WRITE), ("execute", EXECUTE), ("affiliate", AFFILIATE), ("create", CREATE)] 
        where
            -- http://book.realworldhaskell.org/read/using-typeclasses.html
            tryParse [] = []
            tryParse ((attempt, result):xs) =
                if (take (length attempt) value) == attempt
                    then [(result, drop (length attempt) value)]
                    else tryParse xs

instance ToJSON Permission where 
    toJSON = toJSON . show 

instance FromJSON Permission where
    parseJSON (String perm) = return $ read $ unpack perm
    parseJSON _ = error "No parse"

data ActualPrivileges = ActualPrivileges {
    actualPrivilegesGranted :: Set Permission,
    actualPrivilegesDenied :: Set Permission
} deriving (Eq, Show)

instance FromJSON ActualPrivileges where
    parseJSON = withObject "ActualPrivileges" $ \o -> do
        actualPrivilegesGranted <- o .: "granted"
        actualPrivilegesDenied <- o .: "denied"
        return ActualPrivileges{..}

instance ToJSON ActualPrivileges where
    toJSON = toObject [ "granted" .=. actualPrivilegesGranted, "denied" .=. actualPrivilegesDenied ]

data Privileges = Privileges {
    actualPrivileges :: ActualPrivileges,
    effectivePrivileges :: Set Permission
} deriving (Eq, Show)

instance FromJSON Privileges where
    parseJSON = withObject "Privileges" $ \o -> do
        actualPrivileges <- o .: "actual"
        effectivePrivileges <- o .: "effective"
        return Privileges{..}

instance ToJSON Privileges where
    toJSON = toObject [ "actual" .=. actualPrivileges, "effective" .=. effectivePrivileges ]

data ActualClassPrivileges = ActualClassPrivileges {
    actualClassPrivilegesGranted :: HashMap Text (Set Permission),
    actualClassPrivilegesDenied :: HashMap  Text (Set Permission)
} deriving (Eq, Show)

instance FromJSON ActualClassPrivileges where
    parseJSON = withObject "ActualClassPrivileges" $ \o -> do
        actualClassPrivilegesGranted <- o .: "granted"
        actualClassPrivilegesDenied <- o .: "denied"
        return ActualClassPrivileges{..}

instance ToJSON ActualClassPrivileges where
    toJSON ActualClassPrivileges{..} = object [
            "granted" .= actualClassPrivilegesGranted,
            "denied" .= actualClassPrivilegesDenied
        ]

data ClassPrivileges = ClassPrivileges {
    actualClassPrivileges :: ActualClassPrivileges,
    effectiveClassPrivileges :: HashMap Text (Set Permission)
} deriving (Eq, Show)

instance FromJSON ClassPrivileges where
    parseJSON = withObject "ClassPrivileges" $ \o -> do
        actualClassPrivileges <- o .: "actual"
        effectiveClassPrivileges <- o .: "effective"
        return ClassPrivileges{..}

instance ToJSON ClassPrivileges where
    toJSON ClassPrivileges{..} = object [
            "actual" .= actualClassPrivileges,
            "effective" .= effectiveClassPrivileges
        ]
