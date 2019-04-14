module Vendita.MAS.Diagnostics (
    printPretty
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import Vendita.MAS.Core

printPretty :: (ToJSON v) => v -> IO ()
printPretty v = do 
    BS.putStr $ encodePretty v
    putStr "\n"