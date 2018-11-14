Paste the following code into a file called Config.hs in the current directory. Then modify as needed.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Config (serverHost, serverUser, serverPassword) where

import Data.Text (Text)
import Data.ByteString (ByteString)

serverHost :: Text 
serverHost = "host"

serverUser :: ByteString 
serverUser = "user"

serverPassword :: ByteString
serverPassword = "password"
```