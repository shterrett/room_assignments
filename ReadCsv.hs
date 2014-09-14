{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Vector as V
import RoomAssignments
import System.Environment (getArgs)

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
          h:m:_ -> pure (Time h m)
          _ -> empty

splitTimeField :: Field -> [Integer]
splitTimeField = map (\s -> read s :: Integer) . splitOn ":" . BC.unpack

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
formatEvent room evLns event = (name event) ++
                               "," ++ (show (startTime event)) ++
                               "," ++ (show (endTime event)) ++
                               "," ++ (show (attending event)) ++
                               "," ++ (roomId room) ++
                               "\n" ++ evLns

formatSchedule :: Schedule -> String
formatSchedule s = M.foldWithKey formatRoom "Event,Start,End,Attending,Room\n" s

main :: IO ()
main = do
    (eventFile:roomFile:_) <- getArgs
    eventData <- BL.readFile eventFile
    roomData <- BL.readFile roomFile
    let eventList = case decodeByName eventData of
                      Left _ -> []
                      Right (_, v) -> V.toList v

    let roomList = case decodeByName roomData of
                      Left _ -> []
                      Right (_, v) -> V.toList v

    putStrLn (formatSchedule $ scheduleRoomsForEvents eventList roomList)
