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

isCompatibleTests :: [Test]
isCompatibleTests =
    let start = Time 8 30
        end = Time 10 0
    in let smallEvent = Event start end Standard 15
           bigEvent = Event start end Standard 55
           specialEvent = Event start end Science 15
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
    in let firstHour = Event nine ten Standard 10
           secondHour = Event ten eleven Standard 10
           firstHalf = Event nine eleven Standard 10
           secondHalf = Event ten twelve Standard 10
           fullTime = Event nine twelve Standard 10
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
    in let firstHour = Event nine ten Standard 10
           secondHour = Event ten eleven Standard 10
           thirdHour = Event eleven twelve Standard 10
           firstHalf = Event nine eleven Standard 10
           secondHalf = Event ten twelve Standard 10
           fullTime = Event nine twelve Standard 10
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
    in let eventOne = Event one two Standard 10
           eventTwo = Event two three Standard 10
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
    in let eventOne = Event one two Standard 10
           eventTwo = Event two three Standard 10
           eventThree = Event one three Standard 10
           roomOne = Room "one" 15 Standard
           roomTwo = Room "two" 15 Standard
           roomThree = Room "three" 15 Standard
           schedule = Map.fromList [(roomOne, [eventOne, eventTwo]),
                                    (roomTwo, [eventThree])
                                   ]
           testResults s r e = scheduledEvents (scheduleEvent s r e) r
       in [ testCase "adds event to existing list" $
            (Just [eventOne, eventThree]) @=? testResults schedule roomTwo eventOne
          , testCase "adds room and event when not yet in schedule" $
            (Just [eventTwo]) @=? testResults schedule roomThree eventTwo
          , testCase "adds event to list with multiple events" $
            (Just [eventThree, eventOne, eventTwo]) @=? testResults schedule roomOne eventThree
          ]

allTests :: [Test]
allTests = isCompatibleTests ++
           eventOverlapsTests ++
           isAvailableTests ++
           scheduledEventsTests ++
           scheduleEventTests

main :: IO ()
main = exitProperly (runTestTT (TestList allTests))