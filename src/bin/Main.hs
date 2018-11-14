{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO) 
import Network.HTTP.Req (https, (/:))
import Vendita.MAS as MAS
{- 
Yes, you need to provide Config.hs yourself. Should be blindingly obvious
what to do. Take a look at the first line of main below. 
-}
import Config 

main :: IO ()
main = do 
    let server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }
    withServer server $ do
        forms <- listForms []
        liftIO $ forM_ forms print