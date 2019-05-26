{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where 

import qualified Config
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.List (find, isInfixOf, nub)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import Data.UUID.V4 (nextRandom) 
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.HashMap.Strict as M
import qualified Network.HTTP.Client as L
import Network.HTTP.Req
import System.IO
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Diagnostics

defaultServer = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

prefixes :: [Text]
prefixes = ["mas3cloud10", "mas3cloud11", "mas3cloud12", "mas3cloud13", "mas3cloud14", "mas3cloud15", "mas3cloud17"]

main = putStrLn "ok"