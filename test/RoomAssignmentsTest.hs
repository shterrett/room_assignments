import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import RoomAssignments
import Data.Map as Map

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

timeOrdTests :: [Test]
timeOrdTests =
    [ testCase "GT when hour is later" $
      GT @=? compare (Time 10 5) (Time 9 10)
    , testCase "LT when hour is earlier" $
      LT @=? compare (Time 9 10) (Time 10 5)
    , testCase "GT when hours equal and minutes later" $
      GT @=? compare (Time 9 10) (Time 9 5)
    , testCase "LT when hours equal and minutes earlier" $
      LT @=? compare (Time 9 5) (Time 9 10)
    , testCase "EQ when hours and minutes equal" $
      EQ @=? compare (Time 9 8) (Time 9 8)
    ]

eventOrdTests :: [Test]
eventOrdTests =
    let nine = Time 9 0
        ten = Time 10 0
        eleven = Time 11 0
    in [ testCase "GT when start time is later" $
         GT @=? compare (Event "e" ten eleven Standard 10) (Event "e" nine eleven Standard 10)
       , testCase "LT when start time is earlier" $
         LT @=? compare (Event "e" nine ten Standard 10) (Event "e" ten eleven Standard 10)
       , testCase "LT when start times same and attending is lower" $
         LT @=? compare (Event "e" nine ten Standard 10) (Event "e" nine ten Standard 20)
       , testCase "GT when start times same and attending higher" $
         GT @=? compare (Event "e" nine ten Standard 20) (Event "e" nine ten Standard 10)
       , testCase "EQ when start times and attending same" $
         EQ @=? compare (Event "e" nine ten Standard 10) (Event "e" nine eleven Standard 10)
       ]

sortEventListTests :: [Test]
sortEventListTests =
    let nine = Time 9 0
        ten = Time 10 0
        eleven = Time 11 0
        twelve = Time 12 0
    in let third = Event "ten-big" ten twelve Standard 15
           second = Event "ten" ten eleven Standard 10
           fourth = Event "eleven" eleven twelve Standard 10
           first = Event "nine" nine ten Standard 10
       in [ testCase "sorts by time and attending" $
            [first, second, third, fourth] @=? sortEventList [third, second, fourth, first]
          ]


isCompatibleTests :: [Test]
isCompatibleTests =
    let start = Time 8 30
        end = Time 10 0
    in let smallEvent = Event "small" start end Standard 15
           bigEvent = Event "big" start end Standard 55
           specialEvent = Event "special" start end Science 15
           standardRoom = Room "small" 20 Standard
           specialRoom = Room "science" 20 Science
       in [ testCase "standard room with enough seats is compatible" $
            True @=? isCompatible smallEvent standardRoom
          , testCase "standard room with too few seats is incompatible" $
            False @=? isCompatible bigEvent standardRoom
          , testCase "standard room with wrong type is incompatible" $
            False @=? isCompatible specialEvent standardRoom
          , testCase "room with right type and enough seats is compatible" $
            True @=? isCompatible specialEvent specialRoom
          ]

eventOverlapsTests :: [Test]
eventOverlapsTests =
    let nine = Time 9 0
        eleven = Time 11 0
        ten = Time 10 0
        twelve = Time 12 0
    in let firstHour = Event "first" nine ten Standard 10
           secondHour = Event "second" ten eleven Standard 10
           firstHalf = Event "fhalf" nine eleven Standard 10
           secondHalf = Event "shalf" ten twelve Standard 10
           fullTime = Event "full" nine twelve Standard 10
       in [ testCase "starts and ends before" $
            False @=? eventOverlaps firstHour secondHour
          , testCase "starts and ends after" $
            False @=? eventOverlaps secondHour firstHour
          , testCase "starts during and ends after" $
            True @=? eventOverlaps firstHalf secondHalf
          , testCase "starts before and ends during" $
            True @=? eventOverlaps secondHalf firstHalf
          , testCase "completely encompasses" $
            True @=? eventOverlaps fullTime secondHour
          , testCase "completely encompassed" $
            True @=? eventOverlaps secondHour fullTime
          ]


isAvailableTests :: [Test]
isAvailableTests =
    let nine = Time 9 0
        eleven = Time 11 0
        ten = Time 10 0
        twelve = Time 12 0
    in let firstHour = Event "first" nine ten Standard 10
           secondHour = Event "second" ten eleven Standard 10
           thirdHour = Event "third" eleven twelve Standard 10
           firstHalf = Event "fhalf" nine eleven Standard 10
           secondHalf = Event "shalf" ten twelve Standard 10
           fullTime = Event "full" nine twelve Standard 10
       in [ testCase "available when no scheduled events" $
            True @=? isAvailable Nothing firstHour
          , testCase "available when after scheduled event" $
            True @=? isAvailable (Just [firstHalf]) thirdHour
          , testCase "available when before scheduled event" $
            True @=? isAvailable (Just [secondHalf]) firstHour
          , testCase "available when between scheduled events" $
            True @=? isAvailable (Just [firstHour, thirdHour]) secondHour
          , testCase "unavailable when same time as scheduled event" $
            False @=? isAvailable (Just [firstHour, thirdHour]) firstHour
          , testCase "unavailable when contained in scheduled event" $
            False @=? isAvailable (Just [firstHalf, thirdHour]) secondHour
          , testCase "unavailable when totally contained by schedule event" $
            False @=? isAvailable (Just [fullTime]) secondHour
          , testCase "unavailable when partially overlapping" $
            False @=? isAvailable (Just [firstHour, thirdHour]) firstHalf
          , testCase "unavailable when totally contains scheduled event" $
            False @=? isAvailable (Just [secondHour]) fullTime
          , testCase "available if empty list" $
            True @=? isAvailable (Just []) firstHour
          ]

scheduledEventsTests :: [Test]
scheduledEventsTests =
    let one = Time 13 0
        two = Time 14 0
        three = Time 15 0
    in let eventOne = Event "one" one two Standard 10
           eventTwo = Event "two" two three Standard 10
           roomOne = Room "one" 15 Standard
           roomTwo = Room "two" 15 Standard
           schedule = Map.fromList [(roomOne, [eventOne, eventTwo])]
       in [ testCase "returns Nothing if room is not in schedule" $
            Nothing @=? scheduledEvents schedule roomTwo
          , testCase "returns the list of events if room is in schedule" $
            (Just [eventOne, eventTwo]) @=? scheduledEvents schedule roomOne
          ]

scheduleEventTests :: [Test]
scheduleEventTests =
    let one = Time 13 0
        two = Time 14 0
        three = Time 15 0
    in let eventOne = Event "one" one two Standard 10
           eventTwo = Event "two" two three Standard 10
           eventThree = Event "three" one three Standard 10
           roomOne = Room "one" 15 Standard
           roomTwo = Room "two" 15 Standard
           roomThree = Room "three" 15 Standard
           schedule = Map.fromList [(roomOne, [eventOne, eventTwo]),
                                    (roomTwo, [eventThree])
                                   ]
           testResults s r e = scheduledEvents (scheduleEvent s (Just r) e) r
       in [ testCase "adds event to existing list" $
            (Just [eventOne, eventThree]) @=? testResults schedule roomTwo eventOne
          , testCase "adds room and event when not yet in schedule" $
            (Just [eventTwo]) @=? testResults schedule roomThree eventTwo
          , testCase "adds event to list with multiple events" $
            (Just [eventThree, eventOne, eventTwo]) @=? testResults schedule roomOne eventThree
          ]

bestRoomTests :: [Test]
bestRoomTests =
    let one = Time 13 0
        two = Time 14 0
    in let stdEvent = Event "std" one two Standard 20
           sciEvent = Event "sci" one two Science 20
           tooBigEvent = Event "big" one two Standard 100
           roomOne = Room "toosmall" 15 Standard
           roomTwo = Room "scismall" 15 Science
           roomThree = Room "toobig" 30 Standard
           roomFour = Room "scibig" 30 Science
           roomFive = Room "justright" 25 Standard
           roomSix = Room "sciright" 25 Science
           roomList = [roomOne, roomTwo, roomThree, roomFour, roomFive, roomSix]
       in [ testCase "selects smallest compatible room with features" $
            Just roomSix @=? bestRoom sciEvent roomList
          , testCase "selects smallest compatible room without features" $
            Just roomFive @=? bestRoom stdEvent roomList
          , testCase "returns Nothing if no rooms" $
            Nothing @=? bestRoom tooBigEvent roomList
          , testCase "returns Nothing if rooms list is empty" $
            Nothing @=? bestRoom stdEvent []
          ]

availableRoomsTests :: [Test]
availableRoomsTests =
    let one = Time 13 0
        two = Time 14 0
        three = Time 15 0
    in let e1 = Event "1" one two Standard 20
           e2 = Event "2" two three Standard 20
           r1 = Room "1" 25 Standard
           r2 = Room "2" 25 Standard
           r3 = Room "3" 25 Standard
           schedule = Map.fromList [(r1, [e1, e2]),
                                    (r2, [e1])
                                   ]
       in [ testCase "returns rooms that do not have a conflicting event scheduled" $
            [r3] @=? availableRooms schedule e1 [r1, r2, r3]
          , testCase "returns rooms that do not have a conflicting event scheduled" $
            [r2, r3] @=? availableRooms schedule e2 [r1, r2, r3]
          ]

scheduleRoomsForEventsTests :: [Test]
scheduleRoomsForEventsTests =
    let one = Time 13 0
        two = Time 14 0
        three = Time 15 0
    in let e1 = Event "1" one two Standard 20
           e2 = Event "2" two three Standard 20
           e3 = Event "3" two three Standard 30
           e4 = Event "4" one two Science 20
           r1 = Room "1" 25 Standard
           r2 = Room "2" 25 Standard
           r3 = Room "3" 35 Standard
           r4 = Room "4" 35 Science
       in [ testCase "assigns a single room to a single event" $
            Map.fromList [(r1, [e1])] @=? scheduleRoomsForEvents [e1] [r1]
          , testCase "assigns exclusive rooms to events" $
            Map.fromList[(r1, [e1]), (r3, [e3])] @=? scheduleRoomsForEvents [e1, e3] [r1, r3]
          , testCase "assigns rooms to events" $
            Map.fromList [(r1, [e2, e1]), (r3, [e3])] @=? scheduleRoomsForEvents[e3, e1, e2] [r1, r2, r3]
          , testCase "assigns rooms to events with special types" $
            Map.fromList [(r1, [e2, e1]), (r3, [e3]), (r4, [e4])] @=? scheduleRoomsForEvents[e3, e1, e2, e4] [r4, r1, r2, r3]
          ]

allTests :: [Test]
allTests = timeOrdTests ++
           eventOrdTests ++
           isCompatibleTests ++
           eventOverlapsTests ++
           isAvailableTests ++
           scheduledEventsTests ++
           scheduleEventTests ++
           bestRoomTests ++
           availableRoomsTests ++
           scheduleRoomsForEventsTests

main :: IO ()
main = exitProperly (runTestTT (TestList allTests))
