{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where 

import qualified Config
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception (Exception)
import Control.Monad (MonadPlus, liftM, mzero, forM_, forM, when, void)
import Control.Monad.Catch (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (KeyValue, FromJSON, ToJSON, encode, parseJSON, toJSON, Value(String), object, (.=), (.:), withObject, Object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS 
import Data.Char (toLower)
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Typeable (Typeable)
import Data.UUID
import Data.List (intercalate, isPrefixOf, isInfixOf, nub, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import GHC.Generics
import Network.HTTP.Req (https, (/:), HttpException, GET(..), NoReqBody(..), PATCH(..))
import Text.Printf (printf)
import Vendita.MAS
import Vendita.MAS.Diagnostics

defaultServer = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

testInvocation = do 
    let process = "vendita.aws.rds.oracle.checkpoint"
    let parameters = "dbms" $= "mas0000.user.mas.account.aws_oracle_test_vpc"
    invokeNow process parameters >>= checkInvocation . invocationUUID
    where 
        checkInvocation uuid = do
            invocation <- fmap fromJust $ getInvocation uuid
            let status = invocationStatus invocation
            let dateInvoked = invocationDateInvoked invocation
            liftIO $ do
                printf "%s %s %s\n" (show uuid) (show status) (show dateInvoked)
                putStrLn "Delaying 6 seconds…"
                delay 6000000
            if status `elem` [SCHEDULED, EXECUTING, DELAYED]
                then checkInvocation uuid
                else return ()


testSchedule :: MAS ()
testSchedule = do
    connection <- ask
    let process = "vendita.aws.rds.oracle.checkpoint" 
    let schedule = Schedule {
            scheduleName = "fring",
            scheduleDescription = "A temporary schedule I created",
            scheduleProcess = "vendita.aws.rds.oracle.checkpoint",
            scheduleParameters = "dbms" $= "mas0000.user.mas.account.aws_oracle_test_vpc",
            scheduleCrontab = (HOUR @= "*/3") <> (MINUTE @= "15")
        }
    liftIO $ do
        printPretty schedule
        withConnection connection $ deleteSchedule $ scheduleName schedule 

forEach_ :: (a -> IO ()) -> [a] -> IO ()
forEach_ = flip forM_

listAllEntities :: forall r e. (Resource r, FromJSON e) => MAS [e]
listAllEntities = do
    (entities :: [e]) <- withEndpoint @r listAll
    return entities

listResourceEntities :: forall r. (Resource r, FromJSON r) => MAS [r]
listResourceEntities = listAllEntities @r

printEntities :: forall r. (Resource r) => MAS () 
printEntities = listAllEntities @r @Value >>= liftIO . printPretty 

countEntities :: forall r. (Resource r) => MAS Int 
countEntities = fmap length $ listAllEntities @r @Value 

measure :: (MonadIO m) => m a -> m (a, NominalDiffTime)
measure action = do
    start <- liftIO getCurrentTime
    a <- action
    liftIO $ do
        end <- getCurrentTime
        return (a, diffUTCTime end start)

measure_ :: (MonadIO m) => m () -> m NominalDiffTime
measure_ action = do
    (_, diff) <- measure action
    return diff

measureAllRaw = measure (withResource @Process $ withAll $ raw GET NoReqBody)

filterWhere :: v -> (v -> v -> Bool) -> (a -> v) -> [a] -> [a]
filterWhere v compare get = filter (\a -> compare v (get a))

filterOn v get = filterWhere v (==) get
filterIsInfixOf v get = filterWhere v isInfixOf get
filterIsPrefixOf v get = filterWhere v isPrefixOf get

domains :: [Text]
domains = ["mas3cloud10.venditamas.com", "mas3cloud11.venditamas.com", "mas3cloud12.venditamas.com", "mas3cloud13.venditamas.com", "mas3cloud14.venditamas.com", "mas3cloud15.venditamas.com", "mas3cloud17.venditamas.com"]

data PermissionOperation = GRANT | REVOKE | ALLOW | DENY deriving (Eq, Ord, Enum, Read, Show)

changeClassPermission :: Identifier User -> PermissionOperation -> Entity -> MAS Value 
changeClassPermission name op entity = withResource @User $ do
    let configure = withIdentifier name . withPath (pack $ map toLower $ show op) . withPath "create" . withPath (pack $ map toLower $ show entity)
    envelope <- configure $ mas PATCH NoReqBody
    return $ fromJust $ envelopeFirst envelope

main = withServer defaultServer $ do
    revokeMembershipInGroupsFromUser "zinger" ["zing"]
    listAllUsers >>= liftIO . printPretty