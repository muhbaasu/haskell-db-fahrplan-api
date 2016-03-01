{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy                (ByteString, fromStrict)
import Data.Maybe                          (fromJust)
import Data.Text                           (Text, pack)
import Data.Text.Encoding                  (encodeUtf8)
import Data.Time.Calendar                  (Day(..))
import Data.Time.Clock                     (UTCTime)
import Data.Time.ISO8601                   (parseISO8601)
import Data.Time.LocalTime                 (LocalTime(..), TimeZone, TimeOfDay(..), hoursToTimeZone, utcToLocalTime)
import NeatInterpolation
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck               as QC

import Web.DeutscheBahn.API.Schedule
import Web.DeutscheBahn.API.Schedule.Data

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, quickCheckTests]

departureJSON :: ByteString
departureJSON =  fromStrict $ encodeUtf8 [text|
  {
  "name":"RE 15306",
  "type":"RE",
  "stopid":"8000105",
  "stop":"Frankfurt(Main)Hbf",
  "time":"15:01",
  "date":"2016-02-22",
  "direction":"Limburg(Lahn)",
  "track":"3",
  "JourneyDetailRef":{
  "ref":"http://DOMAINE-TOBE-DEFI-NED.de/bin/"
  }
  }|]

-- | +1 UTC non-summer
germanyTimeZone :: TimeZone
germanyTimeZone = hoursToTimeZone 1

departureTime :: LocalTime
departureTime = utcToLocalTime germanyTimeZone departureUTC
  where departureUTC = fromJust $ parseISO8601 "2016-02-22T14:01:00Z"

departure :: Connection
departure = Connection "RE 15306" RE (StopId "8000105") departureTime "Frankfurt(Main)Hbf" "Limburg(Lahn)" "3" ref

refJSON :: ByteString
refJSON = fromStrict $ encodeUtf8 [text|
  {
  "ref":"http://DOMAINE-TOBE-DEFI-NED.de/bin/"
  }|]

ref :: JourneyRef
ref = JourneyRef "http://DOMAINE-TOBE-DEFI-NED.de/bin/"

stopLocationJSON :: ByteString
stopLocationJSON = fromStrict $ encodeUtf8 [text|
{
 "name":"Frankfurt(Main)Hbf",
 "lon":"8.663785",
 "lat":"50.107149",
 "id":"008000105"
}|]

stopCoordinate :: StopCoordinate
stopCoordinate = StopCoordinate 50.107149 8.663785

stopLocation :: StopLocation
stopLocation = StopLocation (StopId "008000105") "Frankfurt(Main)Hbf" stopCoordinate

unitTests = testGroup "Parsing"

  [
    testCase "parse JourneyDetailRef" $
      Right ref @=? (eitherDecode refJSON :: Either String JourneyRef)
  , testCase "parse Departure" $
      Right departure @=? (eitherDecode departureJSON :: Either String Connection)
  , testCase "parse StopLocation" $
      Right stopLocation @=? (eitherDecode stopLocationJSON :: Either String StopLocation)
  ]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

 -- | Seconds always zero
instance Arbitrary TimeOfDay where
  arbitrary = do
    h <- choose (0, 23)
    m <- choose (0, 59)
    return $ TimeOfDay h m 0

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary RouteIndex where
  arbitrary = RouteIndex <$> arbitrary

instance Arbitrary StopCoordinate where
    arbitrary = do
      lat <- choose (-90.0, 90.0)
      lon <- choose (-180.0, 180.0)
      return $ StopCoordinate lat lon

-- Custom instances

instance Arbitrary TransportType where
  arbitrary = elements [ICE, IC, IRE, RE, SBahn]

instance Arbitrary JourneyRef where
  arbitrary = JourneyRef <$> arbitrary

instance Arbitrary StopId where
  arbitrary = StopId <$> arbitrary

instance Arbitrary Connection where
  arbitrary = do
    name          <- arbitrary
    transportType <- arbitrary
    stopId        <- arbitrary
    time          <- arbitrary
    stop          <- arbitrary
    dir           <- arbitrary
    track         <- arbitrary
    ref           <- arbitrary
    return $ Connection name transportType stopId time stop dir track ref

instance Arbitrary Stop where
  arbitrary = do
    id         <- arbitrary
    name       <- arbitrary
    coordinate <- arbitrary
    routeIndex <- arbitrary
    time       <- arbitrary
    track      <- arbitrary
    return $ Stop id name coordinate routeIndex time track

quickCheckTests = testGroup "Parsing and Serialization"
  [ QC.testProperty "serialize/deserialize connection" $
      \connection ->
        Right connection == ((eitherDecode (encode (connection :: Connection))) :: Either String Connection)
  , QC.testProperty "serialize/deserialize stop" $
      \stop ->
        Right stop == ((eitherDecode (encode (stop :: Stop))) :: Either String Stop)
  ]

