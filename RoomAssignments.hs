module RoomAssignments where

import Data.Map as Map

data RoomType = Standard | Science
              deriving (Eq, Show)

data Room = Room {
          roomId    :: String,
          seats :: Integer,
          roomType  :: RoomType
          } deriving (Eq, Show)

instance Ord Room where
    compare a b = compare (roomId a) (roomId b)

type RoomList = [Room]

type Hour = Integer

type Minute = Integer

data Time = Time {
          hour   :: Hour,
          minute ::  Minute
          } deriving (Eq, Show)

instance Ord Time where
    compare t1 t2
        | hour t1 > hour t2 = GT
        | hour t1 < hour t2 = LT
        | hour t1 == hour t2 = compare (minute t1) (minute t2)

data Event = Event {
           startTime    :: Time,
           endTime      :: Time,
           requiredType :: RoomType,
           attending    :: Integer
           } deriving (Eq, Show)

type EventList = [Event]

type Schedule = Map Room [Event]

isCompatible :: Event -> Room -> Bool
isCompatible event room = enoughSeats event room && rightType event room
    where enoughSeats event room = attending event <= seats room
          rightType event room = requiredType event == roomType room

isAvailable :: Maybe EventList -> Event -> Bool
isAvailabe Nothing _ = True
isAvailable (Just elist) e = all (not . eventOverlaps e) elist

scheduledEvents :: Schedule -> Room -> Maybe EventList
scheduledEvents schedule room = Map.lookup room schedule

eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 = within e2 (startTime e1) || within e2 (endTime e1)
    where within event time = time >= (startTime event) && time <= (endTime event)
