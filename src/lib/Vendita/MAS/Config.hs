{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Vendita.MAS.Config (
    NamedServersException(..),
    Tagger,
    allTagged,
    allTaggedSet,
    countMapTagged,
    countTagged,
    countTimeTagged,
    defaultConfigFileName,
    defaultConfigFilePath,
    mapTagged,
    mapTaggedSet,
    readConfig,
    readDefaultConfig,
    tagged,
    taggedSet,
    timeTagged,
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
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow, throwM)
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

withNamedServers :: (MonadIO m, MonadThrow m) => Map String Server -> (String -> MAS a) -> m [a]
withNamedServers servers mas 
    | Map.size servers == 0 = throwM NoNamedServersException
    | otherwise = sequence $ map (\(name, server) -> withServer server (mas name)) (Map.assocs servers)

withNamedServers_ :: (MonadIO m, MonadThrow m) => Map String Server -> (String -> MAS a) -> m ()
withNamedServers_ servers mas = void $ withNamedServers servers mas 

withSelectedNamedServers :: (MonadIO m, MonadThrow m) => Map String Server -> Set String -> (String -> MAS a) -> m [a]
withSelectedNamedServers servers selected mas 
    | Set.size invalidServerNames > 0 = throwM $ InvalidNamedServersException invalidServerNames 
    | otherwise = withNamedServers (Map.restrictKeys servers selected) mas
    where
        invalidServerNames = Set.difference selected (Map.keysSet servers)

withSelectedNamedServers_ :: (MonadIO m, MonadThrow m) => Map String Server -> Set String -> (String -> MAS a) -> m () 
withSelectedNamedServers_ servers selected mas = void $ withSelectedNamedServers servers selected mas 

withSelectedConfiguredServers :: (MonadIO m, MonadThrow m) => Set String -> (String -> MAS a) -> m [a]
withSelectedConfiguredServers selected mas = do 
    config <- liftIO readDefaultConfig 
    withSelectedNamedServers (configServers config) selected mas

withSelectedConfiguredServers_ :: (MonadIO m, MonadThrow m) => Set String -> (String -> MAS a) -> m ()
withSelectedConfiguredServers_ selected mas = void $ withSelectedConfiguredServers selected mas

withConfiguredServer :: (MonadIO m, MonadThrow m) => String -> MAS a -> m a
withConfiguredServer name mas = (!! 0) <$> withSelectedConfiguredServers (Set.singleton name) (const mas)

withConfiguredServer_ :: (MonadIO m, MonadThrow m) => String -> MAS a -> m ()
withConfiguredServer_ name mas = void $ withConfiguredServer name mas

withActiveConfiguredServers :: (MonadIO m, MonadThrow m) => (String -> MAS a) -> m [a] 
withActiveConfiguredServers mas = do
    config <- liftIO readDefaultConfig 
    withSelectedNamedServers (configServers config) (configActives config) mas

withActiveConfiguredServers_ :: (MonadIO m, MonadThrow m) => (String -> MAS a) -> m ()
withActiveConfiguredServers_ mas = void $ withActiveConfiguredServers mas

type Tagger a b = MAS a -> String -> MAS b

allTagged :: Tagger [a] [(String, a)]
allTagged mas name = map (name,) <$> mas

taggedSet :: Ord a => Tagger [a] (String, Set a)
taggedSet mas name = (name,) . Set.fromList <$> mas

mapTaggedSet :: Ord a => Tagger [a] (Map String (Set a))
mapTaggedSet mas name = Map.singleton name . Set.fromList <$> mas

allTaggedSet :: Ord a => Tagger [a] (Set (String, a))
allTaggedSet mas name = Set.fromList . map (name,) <$> mas

tagged :: Tagger a (String, a)
tagged mas name = (name,) <$> mas

mapTagged :: Tagger a (Map String a)
mapTagged mas name = Map.singleton name <$> mas 

countTagged :: Tagger [a] (String, Int)
countTagged mas name = (name,) . length <$> mas 

countMapTagged :: Tagger [a] (Map String Int)
countMapTagged mas name = Map.singleton name . length <$> mas

time :: MAS a -> MAS (a, NominalDiffTime)
time action = do
    start <- liftIO getCurrentTime
    result <- action
    end <- liftIO getCurrentTime
    return $ (result, diffUTCTime end start)

timeTagged :: Tagger a (String, NominalDiffTime)
timeTagged mas name = do 
    (_, diff) <- time mas
    return (name, diff)

countTimeTagged :: Tagger [a] (String, Int, NominalDiffTime)
countTimeTagged mas name = do
    (result, diff) <- time mas
    return (name, length result, diff)