{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where 

import qualified Config
import Control.Exception (Exception)
import Control.Monad (liftM, mzero, forM_, when, void)
import Control.Monad.Catch (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (KeyValue, FromJSON, ToJSON, parseJSON, toJSON, Value, object, (.=), (.:), withObject)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.List (intercalate, isPrefixOf, isInfixOf, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust, fromJust)
import Network.HTTP.Req (https, (/:), HttpException)
import Text.Printf (printf)
import Vendita.MAS
import Vendita.MAS.Diagnostics

server = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

printListWithMessage :: (Resource r, ToJSON r) => String -> MAS [r] -> MAS ()
printListWithMessage message list = do
    resources <- list
    liftIO $ do
        putStrLn $ printf "%s:\n" message
        forM_ resources (liftIO . printPretty)
        putStr "\n"

main = withServer server $ do 
    user <- createUser "watusi" "ichabod" "A watusi" []
    printListWithMessage "After creating watusi" listAllUsers
    deleteUser "watusi" 
    printListWithMessage "After deleting watusi" listAllUsers