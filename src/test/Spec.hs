{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.Maybe
import Data.UUID
import Network.HTTP.Req
import Test.Hspec 
import Vendita.MAS
import qualified Vendita.MAS as MAS
{- 
Yes, you need to provide Config.hs yourself. Should be blindingly obvious
what to do. Take a look at the first line of main below. 
-}
import Config 

main :: IO ()
main = hspec $ before ensureNamespace $ do 
    describe "creating a namespace" $ do
        it "works" $ \namespace -> do
            namespace `shouldBe` "masapi" 

exists = fmap isJust 
namespaceExists = exists . getNamespace

ensureNamespace :: IO String
ensureNamespace = withServer server $ do
    let name = "masapi"
    hasNamespace <- namespaceExists name 
    if hasNamespace
        then return name
        else do
            _ <- createNamespace Namespace{namespaceName="masapi", namespaceDescription="Create MASApi namespace"}
            return name

server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }