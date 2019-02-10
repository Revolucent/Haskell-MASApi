{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import qualified Config
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Network.HTTP.Req (https, (/:))
import Vendita.MAS as MAS

server = Server { 
    MAS.serverUrl = (https Config.serverHost) /: "mas", 
    MAS.serverUser = Config.serverUser, 
    MAS.serverPassword = Config.serverPassword 
}

main = withServer server $ do 
    invocations <- withInvocationDateRange "2019-01-01" 300 listAllInvocations
    liftIO $ print invocations