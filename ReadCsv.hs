{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List.Split (splitOn)
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

main :: IO ()
main = do
    csvData <- BL.readFile "./test/events.csv"
    case decodeByName csvData of
      Left err -> putStrLn err
      Right (_, v) -> V.forM_ v $ \ ev ->
        putStrLn (name ev)