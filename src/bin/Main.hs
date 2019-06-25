{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Diagnostics

taggedMaybe :: Tagger (Maybe a) (Maybe String)
taggedMaybe mas name = do 
    result <- mas
    return $ result >>= return . const name

taggedPositive :: Tagger [a] (Maybe String)
taggedPositive mas name = do
    result <- mas
    if (length result) > 0
        then return $ Just name
        else return Nothing

taggedNegative :: Tagger [a] (Maybe String)
taggedNegative mas name = do
    result <- mas
    if (length result) == 0
        then return $ Just name
        else return Nothing

main = catMaybes 
    <$> withActiveConfiguredServers 
        (taggedNegative 
             $  filter (entityName %== "ventoso.") 
            <$> listProcesses False) 
    >>= mapM putStrLn 