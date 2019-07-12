{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entity.Schedule.Attributes (
    parameters,
    process,
    t_hr,
    t_min
)
where

import Data.Aeson (Value)
import Data.Map (Map)
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Schedule
import Vendita.MAS.Entity.Schedule.Internal.Types
import Vendita.MAS.Strings

data ScheduleAttributeProcess = ScheduleAttributeProcess deriving (Show)

instance ToText ScheduleAttributeProcess where
    toText _ = "process"

instance AttributeKey ScheduleAttributeProcess where 
    type AttributeValue ScheduleAttributeProcess = String
    type AttributeEntity ScheduleAttributeProcess = Schedule

data ScheduleAttributeParameters = ScheduleAttributeParameters deriving (Show)

instance ToText ScheduleAttributeParameters where
    toText _ = "parameters"

instance AttributeKey ScheduleAttributeParameters where
    type AttributeValue ScheduleAttributeParameters = Map String Value
    type AttributeEntity ScheduleAttributeParameters = Schedule

process = ScheduleAttributeProcess
parameters = ScheduleAttributeParameters

t_min :: forall e. ScheduleAttribute e => ScheduleTime e 
t_min = ScheduleTimeMin

t_hr :: forall e. ScheduleAttribute e => ScheduleTime e
t_hr = ScheduleTimeHour