{-# LANGUAGE ScopedTypeVariables #-}

module Vendita.MAS.Diagnostics (
    allp,
    anyp,
    checkServerStatus,
    cmp,
    cmpBy,
    entityHas,
    maybeCmp,
    maybeCmpBy,
    monitor,
    pairs,
    printPretty,
    (%==),
    (*=%=),
    (*===),
    (=%=),
    (===)
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Foldable (Foldable, toList)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Char (toLower)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Form
import Vendita.MAS.Entity.Meta
import Vendita.MAS.Entity.Process
import Vendita.MAS.Entity.Prototype
import Vendita.MAS.Invocation
import Vendita.MAS.Resource

printPretty :: (ToJSON v) => v -> IO ()
printPretty v = do 
    BS.putStr $ encodePretty v
    putStr "\n"

infix 4 *=%=
infix 4 *===
infix 4 =%=
infix 4 ===

cmpBy :: (a -> a) -> (a -> a -> Bool) -> (e -> a) -> a -> e -> Bool
cmpBy transform op = 
    \get value -> 
        \e -> op (transform (get e)) (transform value)

maybeCmpBy :: (a -> a) -> (a -> a -> Bool) -> (e -> Maybe a) -> a -> e -> Bool
maybeCmpBy transform op = 
    \get value -> 
        \e -> fromMaybe False $ 
            (op $ transform value) <$> transform <$> get e

maybeCmp = maybeCmpBy id
cmp = cmpBy id

(===) :: Eq a => (e -> a) -> a -> e -> Bool
(===) = cmp (==) 

(*===) :: Eq a => (e -> Maybe a) -> a -> e -> Bool
(*===) = maybeCmp (==)

(=%=) :: Eq a => (e -> [a]) -> [a] -> e -> Bool
(=%=) = cmp (flip isInfixOf)

(*=%=) :: Eq a => (e -> Maybe [a]) -> [a] -> e -> Bool
(*=%=) = maybeCmp (flip isInfixOf)

(%==) :: Eq a => (e -> [a]) -> [a] -> e -> Bool
(%==) = cmp (flip isPrefixOf)

-- E.g., filter (entityHas processParameters (processParameterName === "foo"))
entityHas :: (Foldable t, Extra x) => (x -> t a) -> (a -> Bool) -> Entity x -> Bool 
entityHas getElems test = any test . getElems . entityExtra 

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

checkServerStatus :: (MonadIO m) => MAS a -> [(Server, String)] -> m () 
checkServerStatus check servers = forM_ servers $ \(server, text) -> do
    withServer server $ catch
        (check >> (liftIO $ printf "UP: %s\n" text))
        (handleHttpException $ liftIO $ printf "DOWN: %s\n" text)
    
pairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]
