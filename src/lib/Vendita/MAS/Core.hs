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
    (.=.),
    Defaultable (..),
    Entity (..),
    Server (..),
    Connection,
    MAS,
    MASTime(..),
    Resource (..),
    DescribedResource (..),
    EnumerationKey(..),
    NamedResource (..),
    withServer,
    Envelope (..),
    defaultPageSize,
    defaultResponseTimeout,
    filterNulls,
    handleHttpStatus,
    handleHttpException,
    handleHttpExceptionJustReturn,
    handleHttpStatusJustReturn,
    handleHttp404JustReturn,
    list,
    first,
    firstResourceWithIdentifier,
    envelopeFirst,
    mas,
    mas_,
    raw,
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
    deleteResourceWithIdentifier_,
    deleteResourceWithIdentifiers_,
    pageSize,
    toKeyValue,
    toObject,
    withConnection,
    withPage,
    withPageSize,
    withPath,
    withOption,
    withIdentifier,
    withIdentifiers,
    listWithIdentifiers,
    listAll,
    toMASTime,
    withAll,
    withEndpoint,
    withResource,
    listResource,
    listAllResource,
    listResourceWithIdentifiers
)
where

import Control.Applicative (empty)
import Control.Exception.Base (Exception, throwIO)
import qualified Control.Exception.Base as E
import Control.Monad.Catch
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Pair, Parser, toJSONKeyText, typeMismatch)
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, isInfixOf, strip, unpack)
import Data.Time
import Data.Time.Format
import Data.Typeable
import Data.UUID
import GHC.Generics
import GHC.Exts (fromList)
import qualified Network.HTTP.Client as C 
import qualified Network.HTTP.Types as T
import Network.HTTP.Req hiding (handleHttpException)
import qualified Network.HTTP.Req as Req 
import Text.Read hiding (get, String)
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

data Entity = ACCOUNT | ALIAS | CONSTANT | EXCEPTION | FORM | NAMESPACE | PROCESS | PROTOTYPE | SCHEDULE | TYPE deriving (Eq, Ord, Enum, Read, Show)

instance EnumerationKey Entity 

instance ToJSON Entity where
    toJSON = enumerationToJSON

instance ToJSONKey Entity where
    toJSONKey = enumerationToJSONKey
   
instance FromJSON Entity where
    parseJSON = enumerationFromJSON

instance FromJSONKey Entity where
    fromJSONKey = enumerationFromJSONKey

toKeyValue :: (ToJSON v) => Text -> (o -> v) -> o -> Pair 
toKeyValue key getValue o = key .= (toJSON $ getValue o)

infixr 5 .=. 
(.=.) :: (ToJSON v) => Text -> (o -> v) -> (o -> Pair)
key .=. getValue = toKeyValue key getValue

toObject :: [o -> Pair] -> o -> Value 
toObject kvs o = object $ map (\kv -> kv o) kvs

filterNulls :: [Pair] -> [Pair]
filterNulls [] = []
filterNulls ((_, Null):ps) = filterNulls ps
filterNulls (p:ps) = (p:(filterNulls ps))

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString } deriving (Eq, Show)

type Connection = (Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = ask >>= liftIO . runReaderT m . t 

instance MonadHttp MAS where
    handleHttpException = throwM 

withConnection :: (MonadIO m) => Connection -> MAS a -> m a
withConnection connection (MAS m) = liftIO $ runReaderT m connection

withServer :: (MonadIO m) => Server -> MAS a -> m a
withServer server m = do
    let accept = header "Accept" "application/json"
    let auth = basicAuth (serverUser server) (serverPassword server)
    withConnection (serverUrl server, (accept <> auth)) m

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

second :: Int
second = 1000000

pageSize :: Int -> Option 'Https
pageSize size = "page_size" =: size

defaultPageSize :: Option 'Https
defaultPageSize = pageSize 4000000 

defaultResponseTimeout :: Option 'Https
defaultResponseTimeout = responseTimeout (600 * second)

class Resource a where
    type Identifier a :: *
    resourceIdentifier :: a -> Identifier a
    resourcePathSegment :: Text 
    resourceOptions :: Option 'Https
    resourceOptions = defaultPageSize <> defaultResponseTimeout

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

listResource :: forall a. (FromJSON a, Resource a) => MAS [a]
listResource = withResource @a $ list get

listResourceWithIdentifiers :: forall a. (FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => [Identifier a] -> MAS [a]
listResourceWithIdentifiers = withResource @a . listWithIdentifiers

list :: (MonadReader Connection m, MonadIO m) => m (Envelope a) -> m [a]
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

firstResourceWithIdentifier :: forall a f. (MonadFail f, FromJSON a, Resource a, Show (Identifier a), Typeable (Identifier a)) => Identifier a -> MAS (f a) 
firstResourceWithIdentifier = withResource @a . firstWithIdentifier

deleteWithIdentifiers_ :: (Show i, Typeable i) => [i] -> MAS ()
deleteWithIdentifiers_ = (flip withIdentifiers) delete_

deleteResourceWithIdentifiers_ :: forall r. (Resource r, Show (Identifier r), Typeable (Identifier r)) => [Identifier r] -> MAS ()
deleteResourceWithIdentifiers_ = withEndpoint @r . deleteWithIdentifiers_

deleteResourceWithIdentifier_ :: forall r. (Resource r, Show (Identifier r), Typeable (Identifier r)) => Identifier r -> MAS ()
deleteResourceWithIdentifier_ identifier = deleteResourceWithIdentifiers_ @r [identifier]

raw :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> MAS ByteString 
raw method body = do
    (url, options) <- ask
    fmap responseBody $ req method url body bsResponse options

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
