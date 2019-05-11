{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where 

import qualified Config
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
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

investigate :: forall r. (Resource r, Show r, FromJSON r) => Bool -> MAS ()
investigate unsummarized = do
    listResourceRaw @r @Value unsummarized >>= liftIO . printPretty
    resources <- listResource @r unsummarized
    liftIO $ if (length resources) > 0
        then forM_ resources print
        else hPrintf stderr "No %ss found." (resourcePathSegment @r)

main = withServer defaultServer $ investigate @Form True 