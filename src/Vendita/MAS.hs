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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vendita.MAS
(
    Server (..),
    Connection,
    MAS,
    withServer,
    Envelope (..),
    Process (..),
    Invocation (..),
    InvocationStatus (..),
    InvocationOutput (..),
    list,
    first,
    mas,
    get,
    post,
    patch,
    put,
    listAll,
    justFirst,
    withPageSize,
    withPath,
    withOption,
    withAccountEndpoint,
    withProcessEndpoint,
    withInvocationEndpoint,
    authenticate,
    listAccounts,
    listForms,
    getForm,
    listNamespaces,
    listProcesses,
    listInvocations,
    listInvocationsFromDay,
    listInvocationsForPeriod,
    getInvocation,
    listInvocationOutputs,
    scheduledInvocationNow,
    scheduleInvocation
)
where

import Control.Concurrent (threadDelay)
import Control.Exception.Base
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default.Class
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Time
import Data.Typeable
import Data.UUID
import GHC.Exts (fromList)
import Network.HTTP.Req
import Text.Read hiding (get)

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString }
type Connection = (Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = fmap t ask >>= liftIO . runReaderT m 

instance MonadHttp MAS where
    handleHttpException = liftIO . throwIO

withServer :: (MonadIO m) => Server -> MAS a -> m a
withServer server (MAS m) = do
    let accept = header "Accept" "application/json"
    let auth = basicAuth (serverUser server) (serverPassword server)
    let defaultPageSize = ("page_size" =: (3500 :: Int))
    liftIO $ runReaderT m (serverUrl server, (accept <> auth <> defaultPageSize))

data Envelope a = Envelope { envelopeContents :: [a], envelopePageCount :: Int, envelopePage :: Int } 

instance (FromJSON a) => FromJSON (Envelope a) where
    parseJSON = withObject "envelope" $ \envelope -> do
        data' <- envelope .: "data" 
        recordField <- data' .: "record_field"
        envelopeContents <- data' .:? recordField .!= []
        envelopePageCount <- data' .:? "page_count" .!= 1
        envelopePage <- data' .:? "page" .!= 1
        return $ Envelope {..} 

data Process = Process { processName :: Text }

instance FromJSON Process where
    parseJSON = withObject "process" $ \process -> do
        processName <- process .: "name"
        return Process{..}

data InvocationStatus = UNKNOWN | SUCCEEDED | FAILED | ABORTED | EXECUTING | SCHEDULED deriving (Enum, Eq, Ord, Show, Read)

instance FromJSON InvocationStatus where
    parseJSON value = do 
        s <- parseJSON value
        case readMaybe s of
            Nothing -> return UNKNOWN 
            Just status -> return status

data Invocation = Invocation { invocationUUID :: UUID, invocationStatus :: InvocationStatus, invocationProcess :: String, invocationDateInvoked :: UTCTime } deriving (Show)

instance FromJSON Invocation where
    parseJSON = withObject "invocation" $ \invocation -> do
        invocationUUID <- invocation .: "uuid"
        invocationStatus <- invocation .: "status"
        invocationProcess <- invocation .: "process"
        s <- invocation .: "date_invoke"
        let invocationDateInvoked = fromJust (parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) s :: Maybe UTCTime)
        return Invocation{..}

data InvocationOutput = InvocationTextOutput InvocationStatus Text | InvocationProgressOutput InvocationStatus Float | InvocationUnknownOutput InvocationStatus deriving (Show) 

data Account = Account { accountUUID :: UUID, accountAddress :: Text, accountProtocol :: Text, accountUser :: Text } deriving (Show)

instance FromJSON Account where
    parseJSON = withObject "account" $ \account -> do
        accountUUID <- account .: "uuid"
        accountAddress <- account .: "address"
        accountProtocol <- account .: "protocol"
        accountUser <- account .: "user"
        return Account{..}

instance FromJSON InvocationOutput where
    parseJSON = withObject "output" $ \output -> do
        status <- output .: "status"
        data' <- output .: "data"
        text <- data' .:? "text"
        case text of
            Just text -> return $ InvocationTextOutput status text
            _ -> do
                progress <- data' .:? "progress"
                case progress of
                    Just progress -> return $ InvocationProgressOutput status progress
                    _ -> return $ InvocationUnknownOutput status

data Namespace = Namespace { namespaceName :: String } deriving (Show)

instance FromJSON Namespace where
    parseJSON = withObject "namespace" $ \nspace -> do
        namespaceName <- nspace .: "name"
        return Namespace{..}

list :: (FromJSON a) => MAS (Envelope a) -> MAS [a] 
list makeRequest = withPage 1 $ do
    envelope <- makeRequest 
    let pageCount = envelopePageCount envelope
    let contents = envelopeContents envelope
    if pageCount <= 1
        then return contents
        else combinePages [2..pageCount] >>= return . (contents ++) 
    where
        setPage :: Int -> Connection -> Connection
        setPage page (url, options) = (url, options <> ("page" =: page))
        withPage p = (local (setPage p))
        combinePages [] = return []
        combinePages (p:ps) = withPage p $ do
            envelope <- makeRequest
            fmap ((envelopeContents envelope) ++) (combinePages ps)

first :: (FromJSON a, MonadFail m) => MAS (Envelope a) -> MAS (m a)
first makeRequest = do
    envelope <- makeRequest
    let contents = envelopeContents envelope
    if (length contents) == 0
        then return $ Fail.fail "NOT FOUND" 
        else return $ return (contents !! 0)

withPageSize :: (MonadReader Connection m) => Int -> m a -> m a
withPageSize pageSize = (local setPageSize)
    where
        setPageSize :: Connection -> Connection
        setPageSize (url, options) = (url, options <> ("page_size" =: pageSize))

withPath :: (MonadReader Connection m) => Text -> m a -> m a
withPath path = (local setPath)
    where
        setPath (url, options) = (url /: path, options)

withOption :: (MonadReader Connection m) => Option 'Https -> m a -> m a
withOption option = (local setOption)
    where
        setOption (url, options) = (url, options <> option)

withFormEndpoint :: (MonadReader Connection m) => m a -> m a
withFormEndpoint = withPath "form"

withProcessEndpoint :: (MonadReader Connection m) => m a -> m a
withProcessEndpoint = withPath "process"

withInvocationEndpoint :: (MonadReader Connection m) => m a -> m a
withInvocationEndpoint = withPath "invocation"

identify :: (Show a, Typeable a) => a -> String
identify a = case (cast a) of
    Just s -> s
    Nothing -> show a

withIdentifiers identifiers m = if (length identifiers) > 0 then withPath (pack (intercalate "," (map identify identifiers))) m else m
withIdentifier identifier = withIdentifiers [identifier]

mas :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, FromJSON a) => method -> body -> MAS a
mas method body = do
    (url, options) <- ask
    res <- req method url body jsonResponse options
    return (responseBody res)
        
get :: (FromJSON a) => MAS a
get = mas GET NoReqBody

post :: (ToJSON a, FromJSON b) => a -> MAS b
post a = mas POST (ReqBodyJson a)

patch :: (ToJSON a, FromJSON b) => a -> MAS b
patch a = mas PATCH (ReqBodyJson a)

put :: (ToJSON a, FromJSON b) => a -> MAS b
put a = mas PUT (ReqBodyJson a)

listAll withContext identifiers = list $ withContext $ withIdentifiers identifiers get
justFirst withContext identifier = fmap fromJust (withContext $ withIdentifier identifier get)

authenticate :: MAS ()
authenticate = withPath "authenticate" $ do 
    (url, options) <- ask
    req POST url NoReqBody ignoreResponse options >> return ()

withAccountEndpoint :: (MonadReader Connection m) => m a -> m a
withAccountEndpoint = withPath "credential"

withNamespaceEndpoint :: (MonadReader Connection m) => m a -> m a
withNamespaceEndpoint = withPath "namespace"

listAccounts :: [UUID] -> MAS [Account]
listAccounts = listAll withAccountEndpoint

getAccount :: UUID -> MAS Account
getAccount = justFirst withAccountEndpoint

listForms :: [UUID] -> MAS [Value]
listForms = listAll withFormEndpoint

getForm :: UUID -> MAS Value
getForm = justFirst withAccountEndpoint

listNamespaces :: [String] -> MAS [Namespace]
listNamespaces = listAll withNamespaceEndpoint

listProcesses :: [String] -> MAS [Process]
listProcesses = listAll withProcessEndpoint

getProcess :: String -> MAS Process
getProcess = justFirst withProcessEndpoint

listInvocations :: [UUID] -> MAS [Invocation]
listInvocations = listAll withInvocationEndpoint

listInvocationsFromDay :: Day -> Integer -> [UUID] -> MAS [Invocation]
listInvocationsFromDay dateInvoke period uuids = do
    let formattedDateInvoke = formatTime defaultTimeLocale "%Y-%m-%d" dateInvoke
    let dateInvokeOption = "date_invoke" =: formattedDateInvoke
    let periodOption = "period" =: period 
    withOption (dateInvokeOption <> periodOption) (listInvocations uuids) 

listInvocationsForPeriod :: Integer -> [UUID] -> MAS [Invocation]
listInvocationsForPeriod period uuids = do
    UTCTime{utctDay=day} <- liftIO getCurrentTime
    let daysAgo = addDays (-1 * period) day
    listInvocationsFromDay daysAgo period uuids

getInvocation :: UUID -> MAS Invocation
getInvocation = justFirst withInvocationEndpoint

listInvocationOutputs :: UUID -> MAS [InvocationOutput]
listInvocationOutputs uuid = withInvocationEndpoint $ withIdentifier uuid $ withPath "display" $ list get

data ScheduledInvocation = ScheduledInvocation String (Maybe UTCTime) Value deriving (Show)

scheduledInvocationNow process = ScheduledInvocation process Nothing

instance ToJSON ScheduledInvocation where
    toJSON (ScheduledInvocation process dateInvoke parameters) = object ["name" .= process, "date_invoke" .= dateInvoke, "parameters" .= parameters]

scheduleInvocation :: ScheduledInvocation -> MAS Invocation
scheduleInvocation scheduledInvocation = fmap fromJust $ first $ withInvocationEndpoint $ post scheduledInvocation 
