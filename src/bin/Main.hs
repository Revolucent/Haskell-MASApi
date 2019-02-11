{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where 

import qualified Config
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isInfixOf)
import Network.HTTP.Req (https, (/:))
import Vendita.MAS
import Vendita.MAS.Diagnostics

server = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

main = withServer server $ do
    createNamespace "greg" "Greg's awesome namespace" >>= liftIO . print
    modifyNamespace "greg" "greg" "Greg's super cool namespace" >>= liftIO . print
    printNames
    deleteNamespace_ "greg"
    deleteNamespace_ "zing"
    printNames
    where
        printNames = do
            names <- map resourceName <$> listAllNamespaces
            forM_ names (liftIO . putStrLn)