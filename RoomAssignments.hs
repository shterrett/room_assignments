module RoomAssignments where

import Data.Map as Map
import Data.List as List

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
        | otherwise = LT

data Event = Event {
           name         :: String,
           startTime    :: Time,
           endTime      :: Time,
           requiredType :: RoomType,
           attending    :: Integer
           } deriving (Eq, Show)

instance Ord Event where
    compare e1 e2
        | startTime e1 > startTime e2 = GT
        | startTime e1 < startTime e2 = LT
        | startTime e1 == startTime e2 = compare (attending e1) (attending e2)
        | otherwise = LT

type EventList = [Event]

type Schedule = Map Room [Event]

sortEventList :: EventList -> EventList
sortEventList = List.sort

isCompatible :: Event -> Room -> Bool
isCompatible event room = enoughSeats event room && rightType event room
    where enoughSeats e r = attending e <= seats r
          rightType e r = requiredType e == roomType r

isAvailable :: Maybe EventList -> Event -> Bool
isAvailable Nothing _ = True
isAvailable (Just elist) e = all (not . eventOverlaps e) elist

scheduledEvents :: Schedule -> Room -> Maybe EventList
scheduledEvents schedule room = Map.lookup room schedule

scheduleEvent :: Schedule -> Room -> Event -> Schedule
scheduleEvent schedule room event =
    let existingEvents = scheduledEvents schedule room in
      Map.insert room (addEvent event existingEvents) schedule
      where addEvent e Nothing = [e]
            addEvent e (Just es) = e:es

eventOverlaps :: Event -> Event -> Bool
eventOverlaps eventOne eventTwo = beginsDuring eventOne eventTwo ||
                                  endsDuring eventOne eventTwo ||
                                  encompasses eventOne eventTwo
    where beginsDuring e1 e2 = startTime e1 >= startTime e2 && startTime e1 < endTime e2
          endsDuring e1 e2 = endTime e1 > startTime e2 && endTime e1 <= endTime e2
          encompasses e1 e2 = startTime e1 < startTime e2 && endTime e1 > endTime e2

bestRoom :: Event -> Maybe RoomList -> Maybe Room
bestRoom _ Nothing = Nothing
bestRoom e (Just rs) = smallest $ List.filter (isCompatible e) rs
    where smallest :: [Room] -> Maybe Room
          smallest [] = Nothing
          smallest rms = Just (List.minimumBy (\r1 r2 -> seats r1 `compare` seats r2) rms)
