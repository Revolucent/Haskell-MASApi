{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO) 
import Network.HTTP.Req (https, (/:))
import Vendita.MAS as MAS
import Config 

main :: IO ()
main = do 
    let server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }
    withServer server $ do
        forms <- listForms []
        liftIO $ forM_ forms print