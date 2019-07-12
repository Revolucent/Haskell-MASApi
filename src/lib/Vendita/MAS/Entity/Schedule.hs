{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Vendita.MAS.Entity.Schedule (
    Schedule,
    ScheduleExtra(..),
    ScheduleTime(..),
    createSchedule,
    deleteSchedule,
    getSchedule,
    listSchedules,
    modifySchedule
)
where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Vendita.MAS.Core
import Vendita.MAS.Entity
import Vendita.MAS.Entity.Attributes
import Vendita.MAS.Entity.Class
import Vendita.MAS.Entity.Extra
import Vendita.MAS.Entity.Schedule.Internal.Types
import Vendita.MAS.Resource
import Vendita.MAS.Strings

data ScheduleAttributeTime
instance ScheduleAttribute ScheduleAttributeTime

data ScheduleTime e = ScheduleTimeMin | ScheduleTimeHour deriving (Enum) 

instance (ScheduleAttribute e) => AttributeKey (ScheduleTime e) where
    type AttributeValue (ScheduleTime e) = String
    type AttributeEntity (ScheduleTime e) = e

instance Show (ScheduleTime e) where
    show ScheduleTimeMin = "t_min"
    show ScheduleTimeHour = "t_hr"

instance ToText (ScheduleTime e) where
    toText ScheduleTimeMin = "t_min"
    toText ScheduleTimeHour = "t_hr"

instance Eq (ScheduleTime e) where
    a == b = (toText a) == (toText b)

instance Ord (ScheduleTime e) where
    compare a b = compare (toText a) (toText b)

data ScheduleExtra = ScheduleExtra {
    scheduleProcess :: String,
    scheduleParameters :: Map String Value,
    scheduleTimes :: Map (ScheduleTime Schedule) String
} deriving (Show)

instance FromJSON ScheduleExtra where
    parseJSON = withObject "schedule" $ \o -> do
        scheduleProcess <- o .: "process"
        scheduleParameters <- o .: "parameters"
        let times = enumFrom ScheduleTimeMin
        scheduleTimes <- Map.fromList . zip times <$> (sequence $ map ((o .:) . toText) times)
        return ScheduleExtra{..}

instance Extra ScheduleExtra where
    extraEntityClass = SCHEDULE

type Schedule = Entity ScheduleExtra
instance ScheduleAttribute Schedule

createSchedule :: Identifier Schedule -> String -> String -> Map String Value -> Attributes ScheduleAttributeTime -> MAS Schedule
createSchedule name description process parameters times = createResource @Schedule $ object $ [
        "name" .= name,
        "description" .= description,
        "process" .= process,
        "parameters" .= parameters
    ] ++ (toPairs times) 

getSchedule :: Identifier Schedule -> MAS Schedule
getSchedule = getResource @Schedule

modifySchedule :: Identifier Schedule -> Attributes Schedule -> MAS Schedule
modifySchedule identifier attributes = modifyResource @Schedule identifier $ object $ toPairs attributes

listSchedules = listResource @Schedule

deleteSchedule = deleteResource @Schedule
