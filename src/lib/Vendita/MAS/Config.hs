{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Vendita.MAS.Config (
    Config(..),
    NamedServersException(..),
    Tagger,
    allTagged,
    defaultConfigFileName,
    defaultConfigFilePath,
    readConfig,
    readDefaultConfig,
    tagged,
    withActiveConfiguredServers,
    withActiveConfiguredServers_,
    withConfiguredServer,
    withConfiguredServer_,
    withNamedServers,
    withNamedServers_,
    withSelectedConfiguredServers,
    withSelectedConfiguredServers_,
    withSelectedNamedServers,
    withSelectedNamedServers_
)
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, SomeException, catch, throwM)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.List ((!!))
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics
import Network.HTTP.Req (HttpException)
import System.Environment
import System.FilePath
import Vendita.MAS.Core

data NamedServersException = NoNamedServersException | InvalidNamedServersException (Set String) deriving (Show, Typeable, Generic)
instance Exception NamedServersException

data Config = Config {
    configServers :: Map String Server,
    configActives :: Set String 
} deriving (Show) 

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        configActives <- o .: "actives"
        configServers <- o .: "servers"
        return Config{..}

defaultConfigFileName :: FilePath
defaultConfigFileName = ".mas.yaml" 

defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = liftA2 (</>) (getEnv "HOME") (pure defaultConfigFileName)

readConfig :: FilePath -> IO Config
readConfig path = BS.readFile path >>= Yaml.decodeThrow

readDefaultConfig :: IO Config
readDefaultConfig = defaultConfigFilePath >>= readConfig 

runConcurrent :: (MonadIO m) => IO a -> m (MVar (Either SomeException a)) 
runConcurrent io = liftIO $ do 
    var <- newEmptyMVar
    forkFinally io (putMVar var)
    return var

withNamedServers :: (MonadIO m, MonadThrow m) => Bool -> Map String Server -> (String -> MAS a) -> m [a]
withNamedServers concurrent servers mas 
    | Map.size servers == 0 = throwM NoNamedServersException
    | concurrent = do
        vars <- sequence $ map (\(name, server) -> runConcurrent (withServer server (mas name))) (Map.assocs servers)
        forM vars $ \var -> do
            result <- liftIO $ takeMVar var
            case result of
                Left e -> throwM e
                Right a -> return a
    | otherwise = sequence $ map (\(name, server) -> withServer server (mas name)) (Map.assocs servers)

withNamedServers_ :: (MonadIO m, MonadThrow m) => Map String Server -> (String -> MAS a) -> m ()
withNamedServers_ servers mas = void $ withNamedServers False servers mas 

withSelectedNamedServers :: (MonadIO m, MonadThrow m) => Bool -> Map String Server -> Set String -> (String -> MAS a) -> m [a]
withSelectedNamedServers concurrent servers selected mas 
    | Set.size invalidServerNames > 0 = throwM $ InvalidNamedServersException invalidServerNames 
    | otherwise = withNamedServers concurrent (Map.restrictKeys servers selected) mas
    where
        invalidServerNames = Set.difference selected (Map.keysSet servers)

withSelectedNamedServers_ :: (MonadIO m, MonadThrow m) => Map String Server -> Set String -> (String -> MAS a) -> m () 
withSelectedNamedServers_ servers selected mas = void $ withSelectedNamedServers False servers selected mas 

withSelectedConfiguredServers :: (MonadIO m, MonadThrow m) => Bool -> Set String -> (String -> MAS a) -> m [a]
withSelectedConfiguredServers concurrent selected mas = do 
    config <- liftIO readDefaultConfig 
    withSelectedNamedServers concurrent (configServers config) selected mas

withSelectedConfiguredServers_ :: (MonadIO m, MonadThrow m) => Set String -> (String -> MAS a) -> m ()
withSelectedConfiguredServers_ selected mas = void $ withSelectedConfiguredServers False selected mas

withConfiguredServer :: (MonadIO m, MonadThrow m) => String -> MAS a -> m a
withConfiguredServer name mas = (!! 0) <$> withSelectedConfiguredServers False (Set.singleton name) (const mas)

withConfiguredServer_ :: (MonadIO m, MonadThrow m) => String -> MAS a -> m ()
withConfiguredServer_ name mas = void $ withConfiguredServer name mas

withActiveConfiguredServers :: (MonadIO m, MonadThrow m) => Bool -> (String -> MAS a) -> m [a] 
withActiveConfiguredServers concurrent mas = do
    config <- liftIO readDefaultConfig 
    withSelectedNamedServers concurrent (configServers config) (configActives config) mas

withActiveConfiguredServers_ :: (MonadIO m, MonadThrow m) => (String -> MAS a) -> m ()
withActiveConfiguredServers_ mas = void $ withActiveConfiguredServers False mas

type Tagger a b = MAS a -> String -> MAS b

allTagged :: (Functor f) => Tagger (f a) (f (String, a)) 
allTagged mas name = fmap (name,) <$> mas

tagged :: Tagger a (String, a)
tagged mas name = (name,) <$> mas
