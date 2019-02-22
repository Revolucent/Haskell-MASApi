{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where 

import qualified Config
import Control.Monad (liftM, mzero, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isPrefixOf, isInfixOf, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust, fromJust)
import Network.HTTP.Req (https, (/:))
import Vendita.MAS
import Vendita.MAS.Diagnostics

server = Server { 
    serverUrl = (https Config.serverHost) /: "mas", 
    serverUser = Config.serverUser, 
    serverPassword = Config.serverPassword 
}

data Name = Name { name :: String, names :: [Name] } deriving (Show)

countName :: Name -> Int
countName name = 1 + (countNames $ names name)

countNames :: [Name] -> Int
countNames [] = 0
countNames ns = sum $ map countName ns

mkNames :: [String] -> [Name]
mkNames = mkNames' . map (splitOn ".")
    where
        mkNames' :: [[String]] -> [Name]
        mkNames' fullyQualifiedNames = do
            (name:rest) <- fullyQualifiedNames
            if (length rest) == 0 then 
                return Name { name = name, names = mkNames' (childrenOf name) }
            else
                mzero
            where
                childrenOf name = do
                    (first:rest) <- fullyQualifiedNames
                    if and [first == name, (length rest) > 0] then
                        return rest
                    else
                        mzero
                        
main = withServer server $ do 
    processes <- sort . map processName . filter hasStepWithMultipleExpressions <$> listAllProcesses
--    process <- fromJust <$> getProcess "vendita.oracle.oda0100.config.check_value"
    liftIO $ do
        forM_ processes putStrLn 
--        print process
    where
        hasStepWithMultipleExpressions :: Process -> Bool 
        hasStepWithMultipleExpressions process = any (\s -> let es = stepExpressions s in (length es) > 1) (processSteps process) 
        hasStep s process = let count = length $ filter (\step -> (stepStep step) == s) $ processSteps process in count > 0
