module Vendita.MAS.Diagnostics (
    examine
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import Vendita.MAS.Core

examine :: MAS ()
examine = withPageSize 1 $ first get >>= liftIO . examineItem
    where
        examineItem :: Maybe Object -> IO ()
        examineItem (Just item) = BS.putStr (encodePretty item) >> putStrLn "\n"
        examineItem _ = putStrLn "No instances of resource available."
