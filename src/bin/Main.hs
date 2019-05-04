{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where 

import qualified Config
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception (Exception)
import Control.Monad (MonadPlus, liftM, mzero, forM_, forM, when, void)
import Control.Monad.Catch (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (KeyValue, FromJSON, ToJSON, encode, parseJSON, toJSON, Value(String), object, (.=), (.:), withObject, Object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS 
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Typeable (Typeable)
import Data.UUID
import Data.List (intercalate, isPrefixOf, isInfixOf, nub, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import GHC.Generics
import Network.HTTP.Req (https, (/:), HttpException, GET(..), NoReqBody(..), PATCH(..))
import Text.Printf (printf)
import Vendita.MAS
import Vendita.MAS.Diagnostics

defaultServer = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

main = withServer defaultServer $ do 
    u :: [Group] <- withResource @Group $ withAll $ list get
    liftIO $ printPretty u