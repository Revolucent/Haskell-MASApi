{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 (pack) 
import Data.List (group, nub, sort)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Diagnostics

-- mas.sys.invocation.notify.create

main = withActiveConfiguredServers_ $ \name -> do 
    let process = "mas.sys.invocation.notify.create"
    whenExists (getProcess process) (const $ liftIO $ putStrLn name) 