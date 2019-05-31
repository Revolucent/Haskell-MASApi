{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Protocol (
    Protocol(..)
)
where

import Vendita.MAS.Resource

data Protocol = Protocol

instance Resource Protocol where
    type Identifier Protocol = String
    resourcePathSegment = "protocol"