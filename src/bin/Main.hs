{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import Control.Applicative
import Control.Conditional (whenM)
import Control.Monad (forM_)
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

examine :: MAS Value
examine = ((!! 0) . envelopeContents) <$> get

createRandomNamespaceName :: Maybe String -> IO String
createRandomNamespaceName parent = do
    uuid <- UUID.toString <$> randomIO
    let name = "__masapi_test__" ++ (filter (/= '-') uuid)
    return $ intercalate "." $ catMaybes [parent, Just name] 

main :: IO ()
main = do 
    let server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }
    createRandomNamespaceName (Just "abc") >>= print