{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vendita.MAS.Daemon (
    isDaemonRunning,
    whenDaemonRunning
)

where

import Control.Monad
import Data.Aeson
import Vendita.MAS.Core

data DaemonStatus = DaemonStatus Bool

instance FromJSON DaemonStatus where
    parseJSON = withObject "daemon status" $ \o -> do
        info <- o .: "data"
        running <- info .: "running"
        return $ DaemonStatus running 

isDaemonRunning :: MAS Bool
isDaemonRunning = do
    (DaemonStatus running) <- withPath "daemon" $ withPath "status" get
    return $ running 

whenDaemonRunning :: MAS a -> MAS () 
whenDaemonRunning mas = do 
    running <- isDaemonRunning
    when running $ void mas

