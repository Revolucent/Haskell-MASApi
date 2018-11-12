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

module Main where

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
import qualified Vendita.MAS.Config.Test as Config

data Server = Server { serverUrl :: Url 'Https, serverUser :: ByteString, serverPassword :: ByteString }
type Connection = (Url 'Https, Option Https)

newtype MAS a = MAS (ReaderT Connection IO a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader Connection MAS where
    ask = MAS ask
    local t (MAS m) = do 
        connection <- ask
        liftIO $ runReaderT m (t connection)

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
            (url, options) <- ask
            envelope <- makeRequest
            liftM ((envelopeContents envelope) ++) (combinePages ps)

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
justFirst withContext identifier = liftM fromJust (withContext $ withIdentifier identifier get)

authenticate :: MAS ()
authenticate = withPath "authenticate" $ do 
    (url, options) <- ask
    req POST url NoReqBody ignoreResponse options >> return ()

listProcesses :: [String] -> MAS [Process]
listProcesses names = listAll withProcessEndpoint names 

getProcess :: String -> MAS Process
getProcess name = justFirst withProcessEndpoint name 

listInvocations :: Day -> Integer -> [UUID] -> MAS [Invocation]
listInvocations dateInvoke period uuids = do
    let formattedDateInvoke = formatTime defaultTimeLocale "%Y-%m-%d" dateInvoke
    let dateInvokeOption = "date_invoke" =: formattedDateInvoke
    let periodOption = "period" =: period 
    withOption (dateInvokeOption <> periodOption) (listAll withInvocationEndpoint uuids)

listInvocationsForPeriod :: Integer -> [UUID] -> MAS [Invocation]
listInvocationsForPeriod period uuids = do
    UTCTime{utctDay=day} <- liftIO getCurrentTime
    let daysAgo = addDays (-1 * period) day
    listInvocations daysAgo period uuids

getInvocation :: UUID -> MAS Invocation
getInvocation uuid = justFirst withInvocationEndpoint uuid

listInvocationOutputs :: UUID -> MAS [InvocationOutput]
listInvocationOutputs uuid = withInvocationEndpoint $ withIdentifier uuid $ withPath "display" $ list get

data ScheduledInvocation = ScheduledInvocation String (Maybe UTCTime) Value deriving (Show)

scheduledInvocationNow process = ScheduledInvocation process Nothing

instance ToJSON ScheduledInvocation where
    toJSON (ScheduledInvocation process dateInvoke parameters) = object ["name" .= process, "date_invoke" .= dateInvoke, "parameters" .= parameters]

scheduleInvocation :: ScheduledInvocation -> MAS Invocation
scheduleInvocation scheduledInvocation = liftM fromJust $ first $ withInvocationEndpoint $ post scheduledInvocation 

refreshInvocation :: Invocation -> MAS Invocation
refreshInvocation i@Invocation{invocationStatus=status}
    | (status == EXECUTING) || (status == SCHEDULED) = do
        liftIO $ threadDelay 1000000
        getInvocation (invocationUUID i) >>= refreshInvocation
    | otherwise = return i

main :: IO ()
main = do
    let server = Server { serverUrl = https Config.serverHost /: "mas", serverUser = Config.serverUser, serverPassword = Config.serverPassword }
    withServer server $ do
        invocations <- listInvocationsForPeriod 90 [] 
        let invocation = invocations !! 0 
        let uuid = invocationUUID invocation
        outputs <- listInvocationOutputs uuid
        liftIO $ forM_ outputs print
        scheduledInvocation <- scheduleInvocation (scheduledInvocationNow "vendita.test_display" (Object $ fromList []))
        liftIO $ print scheduledInvocation