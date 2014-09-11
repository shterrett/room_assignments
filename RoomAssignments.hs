module RoomAssignments where

import Data.Function (on)
import Data.Map as Map
import Data.List as List
import Text.Printf (printf)

data RoomType = Standard | Science
              deriving (Eq, Show)

data Room = Room {
          roomId    :: String,
          seats :: Integer,
          roomType  :: RoomType
          } deriving (Eq, Show)

instance Ord Room where
    compare a b = (compare `on` roomId) a b

type RoomList = [Room]

type Hour = Integer

type Minute = Integer

data Time = Time {
          hour   :: Hour,
          minute ::  Minute
          } deriving (Eq)

instance Show Time where
    show t = (show (hour t)) ++ ":" ++ (printf "%02d" (minute t))

instance Ord Time where
    compare t1 t2 = case (compare `on` hour) t1 t2 of
        EQ -> (compare `on` minute) t1 t2
        ord -> ord

data Event = Event {
           name         :: String,
           startTime    :: Time,
           endTime      :: Time,
           requiredType :: RoomType,
           attending    :: Integer
           } deriving (Eq, Show)

instance Ord Event where
    compare e1 e2 = case (compare `on` startTime) e1 e2 of
                      EQ -> (compare `on` attending) e1 e2
                      ord -> ord

type EventList = [Event]

type Schedule = Map Room [Event]

scheduleRoomsForEvents :: EventList -> RoomList -> Schedule
scheduleRoomsForEvents _ [] = Map.empty
scheduleRoomsForEvents [] _ = Map.empty
scheduleRoomsForEvents es rs = List.foldl scheduleRoomFor Map.empty es
    where scheduleRoomFor s e = scheduleEvent s (bestRoom e (availableRooms s e rs)) e

sortEventList :: EventList -> EventList
sortEventList = List.sort

scheduleEvent :: Schedule -> Maybe Room -> Event -> Schedule
scheduleEvent s Nothing _ = s
scheduleEvent schedule (Just room) event =
    let existingEvents = scheduledEvents schedule room in
      Map.insert room (addEvent event existingEvents) schedule
      where addEvent e Nothing = [e]
            addEvent e (Just es) = e:es

availableRooms :: Schedule -> Event -> RoomList -> RoomList
availableRooms _ _ [] = []
availableRooms s e rs = List.filter (\r -> isAvailable (scheduledEvents s r) e) rs

isAvailable :: Maybe EventList -> Event -> Bool
isAvailable Nothing _ = True
isAvailable (Just elist) e = all (not . eventOverlaps e) elist

scheduledEvents :: Schedule -> Room -> Maybe EventList
scheduledEvents schedule room = Map.lookup room schedule

eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 = beginsDuring || endsDuring || encompasses
    where start1 = startTime e1
          start2 = startTime e2
          end1 = endTime e1
          end2 = endTime e2
          beginsDuring = start1 >= start2 && start1 < end2
          endsDuring = end1 > start2 && end1 <= end2
          encompasses = start1 < start2 && end1 > end2

bestRoom :: Event -> RoomList -> Maybe Room
bestRoom _ [] = Nothing
bestRoom e rs = smallest $ List.filter (isCompatible e) rs
    where smallest :: [Room] -> Maybe Room
          smallest [] = Nothing
          smallest rms = Just (List.minimumBy (\r1 r2 -> (compare `on` seats) r1 r2) rms)

isCompatible :: Event -> Room -> Bool
isCompatible event room = enoughSeats event room && rightType event room
    where enoughSeats e r = attending e <= seats r
          rightType e r = requiredType e == roomType r
