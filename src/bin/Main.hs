{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson hiding ((.=))
import Data.Bits ((.|.))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (toList)
import Data.List (genericLength, group, nub, sort, sortOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid (Endo(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Network.HTTP.Client as C 
import Network.HTTP.Req (HttpException(VanillaHttpException))
import qualified Network.HTTP.Types as T
import System.IO (stderr)
import Text.Printf
import Vendita.MAS
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Account.Attributes
import Vendita.MAS.Entity.Schedule.Attributes
import Vendita.MAS.Diagnostics

main = withConfiguredServer "mas3cloud17" $ do
    deleteSchedule "mas0000.user.mas.schedule.watusi"
    listSchedules False >>= liftIO . print