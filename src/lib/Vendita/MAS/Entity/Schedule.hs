{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Vendita.MAS.Entity.Schedule (
    ScheduleExtra(..),
    Schedule,
    listSchedules
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Resource

data ScheduleExtra = ScheduleExtra deriving (Show)

instance Extra ScheduleExtra where
    extraEntityClass = SCHEDULE 

instance FromJSON ScheduleExtra where
    parseJSON _ = return ScheduleExtra

type Schedule = Entity ScheduleExtra

listSchedules = listResource @Schedule