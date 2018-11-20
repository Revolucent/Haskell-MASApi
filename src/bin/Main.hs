{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import Control.Applicative
import Control.Conditional (whenM)
import Control.Monad (forM_)
import Control.Monad.Catch (catch)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (liftIO) 
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust, catMaybes)
import qualified Data.UUID as UUID
import Network.HTTP.Req (https, (/:))
import System.Random (randomIO)
import Vendita.MAS as MAS
{- 
Yes, you need to provide Config.hs yourself. Should be blindingly obvious
what to do. Take a look at the first line of main below. 
-}
import Config 

printOnHttpException = handleHttpException . print

main :: IO ()
main = do 
    let server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }
    (withServer server $ withPath "foo" get_) `catch` (printOnHttpException "Damn!") 