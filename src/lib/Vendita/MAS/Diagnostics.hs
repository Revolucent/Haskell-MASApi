{-# LANGUAGE ScopedTypeVariables #-}

module Vendita.MAS.Diagnostics (
    allp,
    anyp,
    monitor,
    printPretty,
    processHasParameter,
    processHasParameterWithEditor,
    processHasParameterWithType
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Foldable (Foldable, toList)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Meta
import Vendita.MAS.Entity.Process
import Vendita.MAS.Invocation
import Vendita.MAS.Resource

printPretty :: (ToJSON v) => v -> IO ()
printPretty v = do 
    BS.putStr $ encodePretty v
    putStr "\n"

processHasParameter :: (ProcessParameter -> Bool) -> Process -> Bool
processHasParameter test = any test . processParameters . entityExtra

processHasParameterWithType :: String -> Process -> Bool
processHasParameterWithType dataType = processHasParameter $ \parameter -> (processParameterType parameter) == dataType 

processHasParameterWithEditor :: String -> Process -> Bool
processHasParameterWithEditor editor = processHasParameter $ \parameter -> case metaEditor (processParameterMeta parameter) of
    Just metaEditor -> editor == metaEditor
    _ -> False

data InvocationMonitoringState = Invoke (Identifier Process) InvocationParameters | Monitor UUID

second :: Int = 1000000

monitor :: Identifier Process -> InvocationParameters -> MAS ()
monitor process parameters = monitor' $ Invoke process parameters
    where
        monitor' :: InvocationMonitoringState -> MAS ()
        monitor' (Invoke process parameters) = do
            uuid <- fmap invocationUUID $ invokeNow process parameters
            monitor' $ Monitor uuid
        monitor' m@(Monitor uuid) = do
            -- Check every three seconds
            let delay = 3
            liftIO $ do
                printf "Pausing for %d seconds…\n" delay
                threadDelay $ delay * second
            invocation <- getInvocation uuid
            let printInvocation = liftIO $ print invocation
            case invocationStatus invocation of
                SUCCEEDED -> printInvocation 
                KILLED -> printInvocation
                FAILED -> printInvocation
                _ -> monitor' m

allp :: [a -> Bool] -> a -> Bool
allp [] _ = True
allp (test:tests) a = (test a) && (allp tests a)

anyp :: [a -> Bool] -> a -> Bool
anyp [] _ = True 
anyp (test:[]) a = test a
anyp (test:tests) a = (test a) || (anyp tests a)