{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vendita.MAS
(
    Server (..),
    Connection,
    MAS,
    Resource (..),
    withServer,
    Envelope (..),
    io,
    catch,
    handleHttpStatus,
    list,
    first,
    mas,
    mas_,
    get,
    get_,
    post,
    post_,
    patch,
    patch_,
    put,
    put_,
    createIfNeeded,
    withPageSize,
    withPath,
    withOption,
    withIdentifier,
    withIdentifiers,
    Namespace (..),
    withNamespaceEndpoint,
    listNamespaces,
    getNamespace,
    createNamespace,
    createNamespace_,
    updateNamespace,
    updateNamespace_,
    deleteNamespaces
)
where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (Exception, throwIO)
import qualified Control.Exception.Base as E
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default.Class
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy)
import Data.Text (Text, pack, isInfixOf, strip, unpack)
import Data.Time
import Data.Typeable
import Data.UUID
import GHC.Exts (fromList)
import qualified Network.HTTP.Client as C 
import qualified Network.HTTP.Types as T
import Network.HTTP.Req
import Text.Read hiding (get)

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString }
type Connection = (Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = t <$> ask >>= liftIO . runReaderT m 

instance MonadHttp MAS where
    handleHttpException = liftIO . throwIO

withServer :: (MonadIO m) => Server -> MAS a -> m a
withServer server (MAS m) = do
    let accept = header "Accept" "application/json"
    let auth = basicAuth (serverUser server) (serverPassword server)
    let defaultPageSize = "page_size" =: (3500 :: Int)
    liftIO $ runReaderT m (serverUrl server, (accept <> auth <> defaultPageSize))

data Envelope a = Envelope { envelopeContents :: [a], envelopePageCount :: Int, envelopePage :: Int } deriving (Show)

instance (FromJSON a) => FromJSON (Envelope a) where
    parseJSON = withObject "envelope" $ \envelope -> do
        data' <- envelope .: "data" 
        recordField <- data' .: "record_field"
        envelopeContents <- data' .:? recordField .!= []
        envelopePageCount <- data' .:? "page_count" .!= 1
        envelopePage <- data' .:? "page" .!= 1
        return Envelope{..} 

-- Run a MAS computation inside of the IO monad
io :: MAS a -> (IO a -> IO b) -> MAS b
io (MAS a) t = ask >>= liftIO . t . runReaderT a 

catch :: (Exception e) => MAS a -> (e -> IO a) -> MAS a
catch m handler = io m (`E.catch` handler)

handleHttpStatus :: (Foldable t) => t T.Status -> IO a -> HttpException -> IO a
handleHttpStatus statuses action e@(VanillaHttpException (C.HttpExceptionRequest _ (C.StatusCodeException response _))) = do 
    if elem (C.responseStatus response) statuses
        then action
        else throwIO e
handleHttpStatus _ _ e = throwIO e 

class Resource a where
    type Identifier a
    resourceIdentifier :: a -> Identifier a

list :: (MonadReader Connection m) => m (Envelope a) -> m [a]
list makeRequest = withPage 1 $ do
    envelope <- makeRequest 
    let pageCount = envelopePageCount envelope
    let contents = envelopeContents envelope
    if pageCount <= 1
        then return contents
        else (contents ++) <$> (combinePages [2..pageCount])
    where
        setPage :: Int -> Connection -> Connection
        setPage page (url, options) = (url, options <> ("page" =: page))
        withPage p = local (setPage p)
        combinePages [] = return []
        combinePages (p:ps) = withPage p $ do
            envelope <- makeRequest
            ((envelopeContents envelope) ++) <$> (combinePages ps)

fail404 :: (MonadFail f) => f a
fail404 = Fail.fail "NOT FOUND"

first :: (MonadFail f) => MAS (Envelope a) -> MAS (f a)
first makeRequest = openRequest `catch` (handleHttpStatus [T.status404] (return fail404)) 
    where
        openRequest = do
            envelope <- makeRequest
            let contents = envelopeContents envelope
            if (length contents) == 0
                then return fail404
                else return $ return $ contents !! 0 

withPageSize :: (MonadReader Connection m) => Int -> m a -> m a
withPageSize pageSize = local setPageSize
    where
        setPageSize :: Connection -> Connection
        setPageSize (url, options) = (url, options <> ("page_size" =: pageSize))

withPath :: (MonadReader Connection m) => Text -> m a -> m a
withPath path = local setPath
    where
        setPath (url, options) = (url /: path, options)

withOption :: (MonadReader Connection m) => Option 'Https -> m a -> m a
withOption option = local setOption
    where
        setOption (url, options) = (url, options <> option)

identify :: (Show a, Typeable a) => a -> String
identify a = case (cast a) of
    Just s -> s
    Nothing -> show a

withIdentifiers identifiers m = if (length identifiers) > 0 then withPath (pack (intercalate "," (map identify identifiers))) m else m
withIdentifier identifier = withIdentifiers [identifier]

listWithIdentifiers :: (FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => [Identifier a] -> MAS [a]
listWithIdentifiers = list . (flip withIdentifiers) get

firstWithIdentifier :: (MonadFail f, FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => Identifier a -> MAS (f a) 
firstWithIdentifier = first . (flip withIdentifier) get

create :: (ToJSON a, FromJSON b) => a -> MAS b
create = fmap fromJust . first . post

createIfNeeded :: (Resource r) => r -> (Identifier r -> MAS (Maybe r)) -> (r -> MAS r) -> MAS r
createIfNeeded r getResource createResource = do
    maybeResource <- getResource $ resourceIdentifier r
    case maybeResource of
        Just resource -> return resource
        _ -> createResource r

update :: (ToJSON a, FromJSON b) => a -> MAS b
update = fmap fromJust . first . patch

deleteWithIdentifiers_ = (flip withIdentifiers) delete_

mas :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, FromJSON a) => method -> body -> MAS a
mas method body = do
    (url, options) <- ask
    fmap responseBody $ req method url body jsonResponse options

mas_ :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> MAS () 
mas_ method body = do
    (url, options) <- ask
    req method url body ignoreResponse options >> return ()
        
get :: (FromJSON a) => MAS a
get = mas GET NoReqBody

get_ :: MAS () 
get_ = mas GET NoReqBody

delete :: (FromJSON a) => MAS a
delete = mas DELETE NoReqBody

delete_ :: MAS ()
delete_ = mas_ DELETE NoReqBody 

post :: (ToJSON a, FromJSON b) => a -> MAS b
post = mas POST . ReqBodyJson 

post_ :: (ToJSON a) => a -> MAS () 
post_ = mas_ POST . ReqBodyJson 

patch :: (ToJSON a, FromJSON b) => a -> MAS b
patch = mas PATCH . ReqBodyJson 

patch_ :: (ToJSON a) => a -> MAS () 
patch_ = mas_ PATCH . ReqBodyJson 

put :: (ToJSON a, FromJSON b) => a -> MAS b
put = mas PUT . ReqBodyJson 

put_ :: (ToJSON a) => a -> MAS () 
put_ = mas PUT . ReqBodyJson

data Namespace = Namespace { namespaceName :: String, namespaceDescription :: String } deriving (Show)

instance Resource Namespace where
    type Identifier Namespace = String
    resourceIdentifier = namespaceName

instance FromJSON Namespace where
    parseJSON = withObject "object" $ \obj -> do
        namespaceName <- obj .: "name"
        namespaceDescription <- obj .: "description"
        return Namespace{..}

instance ToJSON Namespace where
    toJSON namespace = object [ "name" .= (namespaceName namespace), "description" .= (namespaceDescription namespace) ]

withNamespaceEndpoint :: (MonadReader Connection m) => m a -> m a
withNamespaceEndpoint = withPath "namespace"

listNamespaces :: [Identifier Namespace] -> MAS [Namespace]
listNamespaces = withNamespaceEndpoint . listWithIdentifiers 

getNamespace :: (MonadFail f) => Identifier Namespace -> MAS (f Namespace)
getNamespace = withNamespaceEndpoint . firstWithIdentifier 

deleteNamespaces :: [Identifier Namespace] -> MAS ()
deleteNamespaces = withNamespaceEndpoint . deleteWithIdentifiers_ 

createNamespace :: Namespace -> MAS Namespace 
createNamespace = withNamespaceEndpoint . create 

createNamespace_ :: Namespace -> MAS ()
createNamespace_ = withNamespaceEndpoint . post_ 

updateNamespace :: Namespace -> MAS Namespace
updateNamespace = withNamespaceEndpoint . update

updateNamespace_ :: Namespace -> MAS ()
updateNamespace_ = withNamespaceEndpoint . patch_ 

