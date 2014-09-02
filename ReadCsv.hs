{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
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
splitTimeField str = map (\s -> read s :: Integer) $ splitOn ":" (show str)

instance FromField RoomType where
    parseField str
      | str == "Standard" = pure Standard
      | str == "Science" = pure Science
      | otherwise = empty

instance FromNamedRecord Room where
    parseNamedRecord r = Room <$> r .: "roomId" <*>
                                  r .: "seats" <*>
                                  r .: "roomType"

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

    print . M.assocs $ scheduleRoomsForEvents eventList roomList
