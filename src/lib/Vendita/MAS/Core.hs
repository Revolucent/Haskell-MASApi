{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vendita.MAS.Core
(
    Defaultable (..),
    Server (..),
    Connection,
    MAS,
    Resource (..),
    DescribedResource (..),
    NamedResource (..),
    withServer,
    Envelope (..),
    handleHttpStatus,
    handleHttpException,
    handleHttpExceptionJustReturn,
    handleHttpStatusJustReturn,
    handleHttp404JustReturn,
    list,
    first,
    envelopeFirst,
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
    delete_,
    deleteWithIdentifiers_,
    deleteResourceWithIdentifiers_,
    withPage,
    withPageSize,
    withPath,
    withOption,
    withIdentifier,
    withIdentifiers,
    listWithIdentifiers,
    listAll,
    withAll,
    withEndpoint,
    withResource,
    listAllResource,
    listResourceWithIdentifiers
)
where

import Control.Exception.Base (Exception, throwIO)
import qualified Control.Exception.Base as E
import Control.Monad.Catch
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, isInfixOf, strip, unpack)
import Data.Time
import Data.Typeable
import Data.UUID
import GHC.Exts (fromList)
import qualified Network.HTTP.Client as C 
import qualified Network.HTTP.Types as T
import Network.HTTP.Req hiding (handleHttpException)
import qualified Network.HTTP.Req as Req 
import Text.Read hiding (get)

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString }

type Connection = (Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = ask >>= liftIO . runReaderT m . t 

instance MonadHttp MAS where
    handleHttpException = throwM 

withServer :: (MonadIO m) => Server -> MAS a -> m a
withServer server (MAS m) = do
    let accept = header "Accept" "application/json"
    let auth = basicAuth (serverUser server) (serverPassword server)
    liftIO $ runReaderT m (serverUrl server, (accept <> auth))

data Envelope a = Envelope { envelopeContents :: [a], envelopePageCount :: Int, envelopePage :: Int } deriving (Show)

instance (FromJSON a) => FromJSON (Envelope a) where
    parseJSON = withObject "envelope" $ \envelope -> do
        data' <- envelope .: "data" 
        recordField <- data' .: "record_field"
        envelopeContents <- data' .:? recordField .!= []
        envelopePageCount <- data' .:? "page_count" .!= 1
        envelopePage <- data' .:? "page" .!= 1
        return Envelope{..} 

handleHttpStatus :: (Foldable t, MonadThrow m) => t T.Status -> (T.Status -> m a) -> HttpException -> m a
handleHttpStatus statuses action e@(VanillaHttpException (C.HttpExceptionRequest _ (C.StatusCodeException response _))) = do 
    let status = C.responseStatus response
    if elem status statuses
        then action status 
        else throwM e 
handleHttpStatus _ _ e = throwM e 

handleHttpStatus_ statuses action e = handleHttpStatus statuses (\_ -> action) e

handleHttpException :: (Monad m) => m a -> HttpException -> m a
handleHttpException m _ = m

handleHttpExceptionJustReturn :: (Monad m) => a -> HttpException -> m a 
handleHttpExceptionJustReturn = handleHttpException . return 

handleHttpStatusJustReturn :: (Foldable t, MonadThrow m) => t T.Status -> a -> HttpException -> m a
handleHttpStatusJustReturn statuses = handleHttpStatus_ statuses . return 

handleHttp404 :: (MonadThrow m) => m a -> HttpException -> m a 
handleHttp404 = handleHttpStatus_ [T.status404]

handleHttp404JustReturn :: (MonadThrow m) => a -> HttpException -> m a
handleHttp404JustReturn = handleHttp404 . return 

class Resource a where
    type Identifier a :: *
    resourceIdentifier :: a -> Identifier a
    resourcePathSegment :: Text 
    resourceOptions :: Option 'Https
    resourceOptions = "page_size" =: (3500 :: Int)

class DescribedResource a where
    resourceDescription :: a -> String

class NamedResource a where
    resourceName :: a -> String

withEndpoint :: forall r a m. (Resource r, MonadReader Connection m) => m a -> m a
withEndpoint = withPath (resourcePathSegment @r)

withResource :: forall r a m. (Resource r, MonadReader Connection m) => m a -> m a
withResource = withEndpoint @r . withOption (resourceOptions @r) 

listAllResource :: forall a. (FromJSON a, Resource a) => MAS [a]
listAllResource = withResource @a listAll

listResourceWithIdentifiers :: forall a. (FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => [Identifier a] -> MAS [a]
listResourceWithIdentifiers = withResource @a . listWithIdentifiers

list :: (MonadReader Connection m) => m (Envelope a) -> m [a]
list makeRequest = do
    envelope <- withPage 1 makeRequest 
    let pageCount = envelopePageCount envelope
    let contents = envelopeContents envelope
    if pageCount <= 1
        then return contents
        else (contents ++) <$> combinePages [2..pageCount]
    where
        combinePages [] = return []
        combinePages (p:ps) = do 
            contents <- envelopeContents <$> withPage p makeRequest
            (contents ++) <$> combinePages ps

fail404 :: (MonadFail f) => f a
fail404 = Fail.fail "NOT FOUND"

first :: (MonadFail f) => MAS (Envelope a) -> MAS (f a)
first makeRequest = openRequest `catch` (handleHttp404JustReturn fail404)
    where
        openRequest = do
            contents <- envelopeContents <$> makeRequest
            if (length contents) == 0
                then return fail404
                else return $ return (contents !! 0)

envelopeFirst :: Envelope a -> a
envelopeFirst envelope = let contents = envelopeContents envelope in contents !! 0

withPage :: (MonadReader Connection m) => Int -> m a -> m a
withPage = withOption . ("page" =:) 

withPageSize :: (MonadReader Connection m) => Int -> m a -> m a
withPageSize = withOption . ("page_size" =:)

withPath :: (MonadReader Connection m) => Text -> m a -> m a
withPath path = local setPath
    where
        setPath (url, options) = (url /: path, options)

withOption :: (MonadReader Connection m) => Option 'Https -> m a -> m a
withOption option = local setOption
    where
        setOption (url, options) = (url, options <> option)

class (Monad m) => Defaultable m where
    (??) :: m a -> a -> a

instance Defaultable Maybe where
    m ?? a = fromMaybe a m 

identify :: (Show a, Typeable a) => a -> String
identify a = (cast a) ?? (show a)  

withIdentifiers identifiers m = if (length identifiers) > 0 then withPath (pack (intercalate "," (map identify identifiers))) m else withPath "*" m
withIdentifier identifier = withIdentifiers [identifier]

withAll m = withPath "*" m

listWithIdentifiers :: (FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => [Identifier a] -> MAS [a]
listWithIdentifiers = list . (flip withIdentifiers) get

listAll :: (FromJSON a) => MAS [a]
listAll = withAll $ list get

firstWithIdentifier :: (MonadFail f, FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => Identifier a -> MAS (f a) 
firstWithIdentifier = first . (flip withIdentifier) get

deleteWithIdentifiers_ :: (Show i, Typeable i) => [i] -> MAS ()
deleteWithIdentifiers_ = (flip withIdentifiers) delete_

deleteResourceWithIdentifiers_ :: forall r. (Resource r, Show (Identifier r), Typeable (Identifier r)) => [Identifier r] -> MAS ()
deleteResourceWithIdentifiers_ = withEndpoint @r . deleteWithIdentifiers_

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
