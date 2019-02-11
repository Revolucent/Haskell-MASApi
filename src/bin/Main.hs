{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import qualified Config
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object)
import Data.HashMap.Strict (keys)
import Data.List (sort)
import Network.HTTP.Req (https, (/:))
import Vendita.MAS

server = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

main = withServer server $ do 
    (envelope :: Envelope Object) <- withPath "prototype" get
    let contents = envelopeContents envelope
    liftIO $ print $ keys (contents !! 0) 