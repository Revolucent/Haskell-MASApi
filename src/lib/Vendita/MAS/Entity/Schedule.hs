{-# LANGUAGE OverloadedStrings #-}

module Vendita.MAS.Entity.Schedule (
    ScheduleExtra(..),
    Schedule
)

where

import Data.Aeson
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra

data ScheduleExtra = ScheduleExtra deriving (Show)

instance Extra ScheduleExtra where
    extraEntityClass = SCHEDULE 

instance FromJSON ScheduleExtra where
    parseJSON _ = return ScheduleExtra

type Schedule = Entity ScheduleExtra
