{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vendita.MAS.Entities.Schedules (
    Schedule (..),
    CronSlot (..),
    Crontab (..),
    (@=),
    createSchedule,
    schedule,
    deleteSchedule,
    getSchedule,
    listAllSchedules
) where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (pack, unpack)
import Vendita.MAS.Core
import Vendita.MAS.Entities.Invocations (Parameters)

data CronSlot = MINUTE | HOUR | DAY | MONTH | WEEKDAY deriving (Eq, Ord, Enum)  

instance Show CronSlot where
    show MINUTE = "t_min"
    show HOUR = "t_hr"
    show DAY = "t_day"
    show MONTH = "t_mon"
    show WEEKDAY = "t_wkd"

instance Read CronSlot where
    readsPrec _ value = tryParse [("t_min", MINUTE), ("t_hr", HOUR), ("t_day", DAY), ("t_mon", MONTH), ("t_wkd", WEEKDAY)] 
        where
            -- http://book.realworldhaskell.org/read/using-typeclasses.html
            tryParse [] = []
            tryParse ((attempt, result):xs) =
                if (take (length attempt) value) == attempt
                    then [(result, drop (length attempt) value)]
                    else tryParse xs

newtype Crontab = Crontab (Map CronSlot String) deriving (Eq, Show, Read, Semigroup, Monoid)

instance ToJSON Crontab where
    toJSON (Crontab crontab) = object $ map (\(key, value) -> (pack $ show key) .= (toJSON value)) $ Map.toList crontab

instance FromJSON Crontab where
    parseJSON = withObject "crontab" (parseSlots (enumFrom $ toEnum 0))
        where
            parseSlots :: [CronSlot] -> Object -> Parser Crontab
            parseSlots [] _ = return mempty
            parseSlots (slot:slots) o = do 
                rest <- parseSlots slots o
                value <- o .:? (pack $ show slot)
                case value of
                    (Just value) -> return $ (slot @= value) <> rest
                    _ -> return rest 

infixr 5 @=

(@=) :: CronSlot -> String -> Crontab
slot @= value = Crontab $ Map.fromList [(slot, value)]

data Schedule = Schedule {
    scheduleName :: String,
    scheduleDescription :: String,
    scheduleProcess :: String,
    scheduleParameters :: Parameters,
    scheduleCrontab :: Crontab
} deriving (Eq, Show)

instance Resource Schedule where
    type Identifier Schedule = String
    resourceIdentifier = scheduleName
    resourcePathSegment = "schedule"

instance NamedResource Schedule where
    resourceName = scheduleName

instance DescribedResource Schedule where
    resourceDescription = scheduleDescription

mergeJSONObjects :: [Value] -> Value 
mergeJSONObjects = object . concat . mergeJSONObjects'
    where
        mergeJSONObjects' :: [Value] -> [[Pair]]
        mergeJSONObjects' values = do
            value <- values
            case value of
                (Object pairs) -> return $ HMap.toList pairs
                _ -> error "Not a JSON object"

instance ToJSON Schedule where
    toJSON schedule = mergeJSONObjects [object [
            "name" .= (scheduleName schedule),
            "description" .= (scheduleDescription schedule),
            "process" .= (scheduleProcess schedule),
            "parameters" .= (scheduleParameters schedule) 
        ], toJSON (scheduleCrontab schedule)]

instance FromJSON Schedule where
    parseJSON = withObject "schedule" $ \o -> do
        scheduleName <- o .: "name"
        scheduleDescription <- o .: "description"
        scheduleProcess <- o .: "process"
        scheduleParameters <- o .: "parameters"
        scheduleCrontab <- parseJSON $ toJSON o
        return Schedule{..}
        
createSchedule :: Schedule -> MAS Schedule 
createSchedule schedule = fmap envelopeFirst $ withEndpoint @Schedule $ post schedule

schedule :: String -> String -> String -> Parameters -> Crontab -> MAS Schedule
schedule name description process parameters crontab = do
    let schedule = Schedule {
            scheduleName = name,
            scheduleDescription = description,
            scheduleProcess = process,
            scheduleParameters = parameters,
            scheduleCrontab = crontab
        }
    createSchedule schedule

listAllSchedules :: MAS [Schedule]
listAllSchedules = listAllResource

getSchedule :: Identifier Schedule -> MAS (Maybe Schedule)
getSchedule = firstResourceWithIdentifier

deleteSchedule :: Identifier Schedule -> MAS ()
deleteSchedule = deleteResourceWithIdentifier_ @Schedule