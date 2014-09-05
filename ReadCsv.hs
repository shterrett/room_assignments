{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Vector as V
import RoomAssignments

instance FromNamedRecord Event where
    parseNamedRecord r = Event <$> r .: "name" <*>
                                   r .: "startTime" <*>
                                   r .: "endTime" <*>
                                   r .: "requiredType" <*>
                                   r .: "attending"

instance FromField Time where
    parseField str =
      let time = splitTimeField str in
        case time of
          h:m:s -> pure (Time h m)
          otherwise -> empty

splitTimeField :: Field -> [Integer]
splitTimeField str = map (\s -> read s :: Integer) $ splitOn ":" (BC.unpack str)

instance FromField RoomType where
    parseField str
      | str == "Standard" = pure Standard
      | str == "Science" = pure Science
      | otherwise = empty

instance FromNamedRecord Room where
    parseNamedRecord r = Room <$> r .: "roomId" <*>
                                  r .: "seats" <*>
                                  r .: "roomType"

formatRoom :: Room -> EventList -> String -> String
formatRoom room events text = text ++ (foldl (formatEvent room) ""  events)

formatEvent :: Room -> String -> Event -> String
formatEvent room lines event = (name event) ++
                               "," ++ (show (startTime event)) ++
                               "," ++ (show (endTime event)) ++
                               "," ++ (show (attending event)) ++
                               "," ++ (roomId room) ++
                               "\n" ++ lines

formatSchedule :: Schedule -> String
formatSchedule s = M.foldWithKey formatRoom "Event,Start,End,Attending,Room\n" s

main :: IO ()
main = do
    eventData <- BL.readFile "./test/events.csv"
    roomData <- BL.readFile "./test/rooms.csv"
    let eventList = case decodeByName eventData of
                      Left _ -> []
                      Right (_, v) -> V.toList v

    let roomList = case decodeByName roomData of
                      Left _ -> []
                      Right (_, v) -> V.toList v

    writeFile "./test/schedule.csv" (formatSchedule $ scheduleRoomsForEvents eventList roomList)
