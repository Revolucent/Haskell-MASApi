{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative (empty, liftA2, (<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString.Char8 (pack) 
import Data.List (group, nub, sortOn)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.UUID as UUID 
import Data.UUID (UUID)
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Diagnostics

main = withActiveConfiguredServers_ $ \name -> do 
    liftIO $ putStrLn name
    (getProcess "ventoso.foo" `when404` getProcess "mas.sys.invocation.notify.create") >>= liftIO . putStrLn . entityName

    
