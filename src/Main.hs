{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO) 
import Network.HTTP.Req (https, (/:))
import Vendita.MAS 
import qualified Vendita.MAS.Config.Test as Config

main :: IO ()
main = do 
    let server = Server { serverUrl = (https Config.serverHost) /: "mas", serverUser = Config.serverUser, serverPassword = Config.serverPassword }
    withServer server $ do
        namespaces <- listNamespaces [] 
        liftIO $ forM_ namespaces print