{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.List (intercalate)
import Data.Maybe
import Data.UUID as UUID
import Network.HTTP.Req
import System.Random
import Test.Hspec 
import Vendita.MAS
import qualified Vendita.MAS as MAS
{- 
Yes, you need to provide Config.hs yourself. Should be blindingly obvious
what to do. Take a look at the first line of main below. 
-}
import Config 

{-

TEST PLAN:

Create namespace from UUID.
Pass it along.
Destroy it at the end of all tests.



-}

main :: IO ()
main = hspec $ around withRandomNamespace $ do 
    describe "creating a namespace" $ do
        it "works" $ \testNamespace -> do
            let testNamespaceName = namespaceName testNamespace
            subnamespaceName <- createRandomNamespaceName (Just testNamespaceName) 
            let subnamespace = Namespace{namespaceName=subnamespaceName, namespaceDescription=subnamespaceName}
            withServer server $ do
                createNamespace subnamespace
                deleteNamespaces [resourceIdentifier subnamespace]

createRandomNamespaceName :: Maybe String -> IO String
createRandomNamespaceName parent = do
    uuid <- UUID.toString <$> randomIO
    let name = "__masapi_test__" ++ (filter (/= '-') uuid)
    return $ intercalate "." $ catMaybes [parent, Just name] 

createRandomNamespace :: IO Namespace
createRandomNamespace = do
    name <- createRandomNamespaceName Nothing
    let namespace = Namespace{namespaceName=name, namespaceDescription=name}
    withServer server $ createNamespace namespace

deleteRandomNamespace :: Namespace -> IO ()
deleteRandomNamespace namespace = withServer server $ deleteNamespaces [namespaceName namespace]

withRandomNamespace = bracket createRandomNamespace deleteRandomNamespace

server = Server { MAS.serverUrl = (https Config.serverHost) /: "mas", MAS.serverUser = Config.serverUser, MAS.serverPassword = Config.serverPassword }