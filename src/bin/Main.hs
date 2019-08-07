{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson 
import Control.Applicative (liftA2, (<|>))
import Control.Monad (forM_, liftM, mzero, void, when)
import Control.Monad.Catch (catch)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Writer
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (elemIndex, group, nub, sort, sortOn, isPrefixOf)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, isJust, isNothing, fromMaybe, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import qualified Data.UUID as UUID 
import qualified Data.Vector as Vector 
import Data.Vector (Vector)
import Prelude hiding (log)
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Schedule.Attributes
import Vendita.MAS.Diagnostics (allp, entityHas, printPretty, (===), (%==), (=%=), (==%), (*===))

log :: String -> WriterT [String] MAS ()
log message = do
    now <- formatTime defaultTimeLocale "%FT%T" <$> liftIO getCurrentTime
    let logMessage :: String = printf "[%s] %s" now message 
    tell [logMessage]

run :: WriterT [String] MAS () -> MAS [String]
run something = execWriterT $ do 
    log "Starting"
    running <- lift isDaemonRunning
    if not running
        then log "Daemon not running"
        else something 
    log "Finished"

printLog :: [(String, [String])] -> IO ()
printLog logs = forM_ logs $ \entry -> do
    let (server, entries) = entry
    forM_ entries $ \s -> do
        printf "%s %s\n" server s

data Monitor = Monitor {
        monitoredInvocation :: Invocation,
        monitoringInvocation :: Invocation 
    } deriving (Show)

instance FromJSON Monitor where
    parseJSON = withObject "monitor" $ \o -> do
        monitoredInvocation <- o .: "process"
        monitoringInvocation <- o .: "monitor"
        return Monitor{..}

decodeString :: (FromJSON a) => String -> Maybe a
decodeString = decode . BS.pack

maybeResult :: Result a -> Maybe a
maybeResult result = case result of
    Success a -> Just a
    _ -> Nothing

getStringsFromValue :: Value -> Maybe [String]
getStringsFromValue value = flattenStrings value >>= maybeResult . fromJSON . Array . Vector.fromList 
    where
        flattenStrings :: Value -> Maybe [Value]
        flattenStrings (Array values) = fmap concat $ sequence $ map flattenStrings $ Vector.toList values
        flattenStrings value@(String _) = Just [value]
        flattenStrings _ = Nothing

main = withActiveConfiguredServers True (tagged $ map entityName . filter (entityHas processParameters (processParameterType === "dict")) <$> listProcesses True) >>= mapM_ print