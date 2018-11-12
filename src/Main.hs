module Main where 

import Control.Monad.IO.Class (liftIO) 
import Network.HTTP.Req (https)
import Vendita.MAS (serverUrl, serverUser, serverPassword)
import qualified Vendita.MAS as MAS
import qualified Vendita.MAS.Config.Test as Config

main :: IO ()
main = do 
    let server = MAS.Server { serverUrl = https Config.serverHost, serverUser = Config.serverUser, serverPassword = Config.serverPassword }
    MAS.withServer server $ do
        invocations <- MAS.listInvocationsForPeriod 90 []
        liftIO $ print $ length invocations