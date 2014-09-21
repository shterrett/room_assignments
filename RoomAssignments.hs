module RoomAssignments where

import Data.Function (on)
import Data.Map as Map
import Data.Monoid
import Data.List as List
import Text.Printf (printf)

data RoomType = Standard | Science
              deriving (Eq, Show)

data Room = Room {
          roomId    :: String,
          seats :: Integer,
          roomType  :: RoomType
          } deriving (Show)

instance Eq Room where
    (==) a b = roomId a == roomId b

instance Ord Room where
    compare = (compare `on` roomId)

type RoomList = [Room]

type Hour = Integer

type Minute = Integer

data Time = Time {
          hour   :: Hour,
          minute ::  Minute
          } deriving (Eq, Ord)

instance Show Time where
    show t = (show (hour t)) ++ ":" ++ (printf "%02d" (minute t))

data Event = Event {
           name         :: String,
           startTime    :: Time,
           endTime      :: Time,
           requiredType :: RoomType,
           attending    :: Integer
           } deriving (Eq, Show)

instance Ord Event where
    compare = (compare `on` startTime) <> (compare `on` attending)

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
      where addEvent e [] = [e]
            addEvent e es = e:es

availableRooms :: Schedule -> Event -> RoomList -> RoomList
availableRooms _ _ [] = []
availableRooms s e rs = List.filter (\r -> isAvailable (scheduledEvents s r) e) rs

isAvailable :: EventList -> Event -> Bool
isAvailable [] _ = True
isAvailable elist e = all (not . eventOverlaps e) elist

scheduledEvents :: Schedule -> Room -> EventList
scheduledEvents s r = Map.findWithDefault [] r s

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
