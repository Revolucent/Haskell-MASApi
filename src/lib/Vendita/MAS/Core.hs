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
    Connection,
    EnumerationKey(..),
    Envelope (..),
    MAS,
    MASTime(..),
    Server (..),
    askServer,
    defaultPageSize,
    defaultResponseTimeout,
    delete_,
    envelopeFirst,
    filterNulls,
    first,
    fromMaybe404,
    fromMaybeHttpException,
    fromMaybeHttpStatus,
    get,
    get_,
    handleHttp404,
    handleHttp404JustReturn,
    handleHttpException,
    handleHttpExceptionJustReturn,
    handleHttpStatus,
    handleHttpStatusJustReturn,
    list,
    mapServer,
    mapServers,
    mas,
    mas_,
    maybe404,
    maybeHttpException,
    maybeHttpStatus,
    pageSize,
    patch,
    patch_,
    post,
    post_,
    put,
    put_,
    raw,
    toMASTime,
    whenExists,
    when404,
    when404_,
    withConnection,
    withEnumeration,
    withOption,
    withPage,
    withPageSize,
    withPath,
    withServer,
    withServers
)
where

import Control.Applicative (Alternative, empty, liftA2, (<|>))
import Control.Exception.Base (Exception, throwIO)
import qualified Control.Exception.Base as E
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser, toJSONKeyText, typeMismatch)
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Text (Text, pack, isInfixOf, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Format
import Data.Typeable
import Data.UUID
import GHC.Generics
import GHC.Exts (fromList)
import qualified Network.HTTP.Client as C 
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Client as L
import Network.HTTP.Req hiding (handleHttpException)
import qualified Network.HTTP.Req as Req 
import System.IO.Unsafe (unsafePerformIO)
import Text.Read hiding (get, String, lift)
import Text.Printf

newtype MASTime = MASTime { utcTime :: UTCTime } deriving (Eq, Ord)

toMASTime :: UTCTime -> MASTime
toMASTime t = MASTime { utcTime = t }

instance Show MASTime where
    show = show . utcTime 

instance FormatTime MASTime where
    formatCharacter c = fmap (\f locale mpado i t -> f locale mpado i (utcTime t)) (formatCharacter c)

instance ParseTime MASTime where
    buildTime locale cs = fmap toMASTime (buildTime locale cs)

instance ToJSON MASTime where
    toJSON MASTime{..} = toJSON $ formatTime defaultTimeLocale "%FT%T" utcTime

instance FromJSON MASTime where
    parseJSON (String s) = parseTimeM True defaultTimeLocale "%FT%T" $ unpack s 
    parseJSON _ = empty

class (Read a, Show a, ToJSON a, FromJSON a, FromJSONKey a) => EnumerationKey a where
    errorMessage :: String 
    errorMessage = "invalid"
    enumerationToJSON :: a -> Value 
    enumerationToJSON = toJSON . map toLower . show
    enumerationToJSONKey :: ToJSONKeyFunction a 
    enumerationToJSONKey = toJSONKeyText (pack . map toLower . show)
    enumerationFromJSON :: Value -> Parser a
    enumerationFromJSON invalid@(String s) = case readMaybe $ map toUpper $ unpack s of
        Just value -> return value
        _ -> typeMismatch (errorMessage @a) invalid
    enumerationFromJSON invalid = typeMismatch (errorMessage @a) invalid
    enumerationFromJSONKey :: FromJSONKeyFunction a
    enumerationFromJSONKey = FromJSONKeyText (read . map toUpper . unpack)

withEnumeration :: (MonadReader Connection m, EnumerationKey k) => k -> m a -> m a
withEnumeration = withPath . pack . map toLower . show

filterNulls :: [Pair] -> [Pair]
filterNulls [] = []
filterNulls ((_, Null):ps) = filterNulls ps
filterNulls (p:ps) = (p:(filterNulls ps))

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString } deriving (Eq, Show)

instance FromJSON Server where
    parseJSON = withObject "server" $ \o -> do
        path <- o .: "path"
        domain <- o .: "domain"
        let serverUrl = (https domain) /: path
        serverUser <- encodeUtf8 <$> o .: "user"
        serverPassword <- encodeUtf8 <$> o .: "password"
        return Server{..}

type Connection = (Server, Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = ask >>= liftIO . runReaderT m . t 
    
askServer :: MonadReader Connection m => m Server
askServer = do 
    (server, _, _) <- ask
    return server

instance MonadHttp MAS where
    handleHttpException = throwM 

instance MonadPlus MAS where
    mzero = liftIO mzero
    mplus m1 m2 = catch m1 (\(e :: SomeException) -> m2)

instance Alternative MAS where
    empty = mzero 
    (<|>) = mplus

withConnection :: (MonadIO m) => Connection -> MAS a -> m a
withConnection connection (MAS m) = liftIO $ runReaderT m connection

withServer :: (MonadIO m) => Server -> MAS a -> m a
withServer server m = do
    let accept = header "Accept" "application/json"
    let auth = basicAuth (serverUser server) (serverPassword server)
    withConnection (server, serverUrl server, (accept <> auth)) m

withServers :: (MonadIO m, Traversable t) => t Server -> MAS a -> m (t a) 
withServers servers m = forM servers ((flip withServer) m) 

mapServer :: (MonadIO m) => MAS a -> Server -> m a
mapServer = flip withServer

mapServers :: (MonadIO m, Traversable t) => MAS a -> t Server -> m (t a) 
mapServers action = mapM (mapServer action)

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

maybeHttpStatus :: (Foldable t, MonadCatch m) => t T.Status -> m a -> m (Maybe a)
maybeHttpStatus statuses attempt = catch (Just <$> attempt) (handleHttpStatusJustReturn statuses Nothing)

fromMaybeHttpStatus :: (Foldable t, MonadCatch m) => a -> t T.Status -> m a -> m a
fromMaybeHttpStatus deflt statuses attempt = fromMaybe deflt <$> maybeHttpStatus statuses attempt

maybe404 :: (MonadCatch m) => m a -> m (Maybe a)
maybe404 = maybeHttpStatus [T.status404]

fromMaybe404 :: (MonadCatch m) => a -> m a -> m a
fromMaybe404 deflt attempt = fromMaybe deflt <$> maybe404 attempt

maybeHttpException :: (MonadCatch m) => m a -> m (Maybe a)
maybeHttpException attempt = catch (Just <$> attempt) (handleHttpExceptionJustReturn Nothing)

fromMaybeHttpException :: (MonadCatch m) => a -> m a -> m a
fromMaybeHttpException deflt attempt = fromMaybe deflt <$> maybeHttpException attempt

infixl 3 `when404`

when404 :: (MonadCatch m) => m a -> m a -> m a
when404 attempt failure = catch attempt (handleHttp404 failure)

when404_ :: (MonadCatch m) => m a -> m b -> m ()
when404_ attempt failure = catch (void attempt) (handleHttp404 (void failure))

whenExists :: (MonadCatch m) => m a -> (a -> m b) -> m () 
whenExists attempt action = void $ runMaybeT $ (MaybeT $ maybe404 attempt) >>= lift . void . action 

second :: Int
second = 1000000

pageSize :: Int -> Option 'Https
pageSize size = "page_size" =: size

defaultPageSize :: Option 'Https
defaultPageSize = pageSize 4000000 

defaultResponseTimeout :: Option 'Https
defaultResponseTimeout = responseTimeout (600 * second)

list :: (MonadReader Connection m, MonadIO m) => m (Envelope a) -> m [a]
list makeRequest = do
    envelope <- withPage 1 makeRequest 
    let pageCount = envelopePageCount envelope
    let contents = envelopeContents envelope
    if pageCount <= 1
        then return contents
        else (contents ++) <$> combinePages [2..pageCount]
    where
        combinePages [] = pure []
        combinePages (p:ps) = liftA2 (++) (envelopeContents <$> withPage p makeRequest) (combinePages ps)

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
        setPath (server, url, options) = (server, url /: path, options)

withOption :: (MonadReader Connection m) => Option 'Https -> m a -> m a
withOption option = local setOption
    where
        setOption (server, url, options) = (server, url, options <> option)

raw :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> MAS ByteString 
raw method body = do
    (_, url, options) <- ask
    fmap responseBody $ req method url body bsResponse options

mas :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, FromJSON a) => method -> body -> MAS a
mas method body = do
    (_, url, options) <- ask
    fmap responseBody $ req method url body jsonResponse options

mas_ :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> MAS () 
mas_ method body = do
    (_, url, options) <- ask
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
