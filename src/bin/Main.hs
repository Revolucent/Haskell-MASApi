{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bits ((.|.))
import Data.Foldable (toList)
import Data.List (genericLength, sortOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Network.HTTP.Client as C 
import Network.HTTP.Req (HttpException(VanillaHttpException))
import qualified Network.HTTP.Types as T
import System.IO (stderr)
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Diagnostics

whenM :: (Monad m) => (m Bool) -> m () -> m ()
whenM test action = test >>= ((flip when) action)

exception :: MAS a -> MAS (Either T.Status a)
exception mas = (Right <$> mas) `catch` (\(VanillaHttpException (C.HttpExceptionRequest _ (C.StatusCodeException response _))) -> return $ Left $ C.responseStatus response)

benchmark :: MAS a -> MAS (NominalDiffTime, a)
benchmark mas = do
    start <- liftIO getCurrentTime
    result <- mas
    end <- liftIO getCurrentTime
    return (diffUTCTime end start, result) 

data Event = START | COMPLETED | ABNORMAL deriving (Eq, Ord, Enum, Show)

bits :: Event -> Int
bits START = 1
bits COMPLETED = 2
bits ABNORMAL = 4

monitor :: (Foldable f, Functor f, Foldable t, ToJSON (t String)) => String -> Map String Value -> f Event -> t String -> Maybe UTCTime -> MAS Invocation 
monitor name parameters events addresses maybeWhen = do
    let params = "name" =# name 
            <> "parameters" =# parameters 
            <> "events" =# (foldr (.|.) 0 $ fmap bits events) 
            <> "addresses" =# addresses 
            <> (fromMaybe mempty $ ("when" =#) . formatTime defaultTimeLocale "%FT%T" <$> maybeWhen)
    pollNow "mas.sys.invocation.notify.create" params Nothing

time :: (MonadIO m) => m a -> m (a, NominalDiffTime)
time action = do
    start <- liftIO getCurrentTime 
    result <- action
    end <- liftIO getCurrentTime
    return (result, end `diffUTCTime` start)

mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (fa, fb) (a, b) = (fa a, fb b)

average xs = realToFrac (sum xs) / genericLength xs

main = withActiveConfiguredServers True (tagged countActivity) 
    >>= return . reverse . sortOn snd
    >>= mapM print
    where
        countActivity :: MAS Int
        countActivity = do
            start <- addDays (-90) . utctDay <$> liftIO getCurrentTime
            liftA2 (+) 
                (length . filter custom <$> listEntities False)
                (length <$> listInvocations start 180 False)
        custom :: Generic -> Bool
        custom = not . anyp [entityUserOwner === "mas", entityUserOwner === "postgres", entityUserOwner %== "mas_"]