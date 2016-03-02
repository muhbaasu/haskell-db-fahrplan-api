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

stopCoordinate :: Coordinate
stopCoordinate = Coordinate 50.107149 8.663785

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

instance Arbitrary Coordinate where
    arbitrary = do
      lat <- choose (-90.0, 90.0)
      lon <- choose (-180.0, 180.0)
      return $ Coordinate lat lon

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

instance Arbitrary CoordLocation where
  arbitrary = do
    name    <- arbitrary
    locType <- arbitrary
    coord   <- arbitrary
    return $ CoordLocation name locType coord

instance Arbitrary Journey where
  arbitrary = do
    stops     <- arbitrary
    names     <- arbitrary
    types     <- arbitrary
    operators <- arbitrary
    notes     <- arbitrary
    return $ Journey stops names types operators notes

instance Arbitrary JourneyType where
  arbitrary = do
    transportType <- arbitrary
    routeIdxFrom  <- arbitrary
    routeIdxTo    <- arbitrary
    return $ JourneyType transportType routeIdxFrom routeIdxTo

instance Arbitrary Operator where
  arbitrary = do
    name          <- arbitrary
    routeIdxFrom  <- arbitrary
    routeIdxTo    <- arbitrary
    return $ Operator name routeIdxFrom routeIdxTo

instance Arbitrary Name where
  arbitrary = do
    name         <- arbitrary
    routeIdxFrom <- arbitrary
    routeIdxTo   <- arbitrary
    return $ Name name routeIdxFrom routeIdxTo

instance Arbitrary Note where
  arbitrary = do
    key          <- arbitrary
    priority     <- arbitrary
    routeIdxFrom <- arbitrary
    routeIdxTo   <- arbitrary
    return $ Note key priority routeIdxFrom routeIdxTo

instance Arbitrary Stop where
  arbitrary = do
    id         <- arbitrary
    name       <- arbitrary
    coordinate <- arbitrary
    routeIndex <- arbitrary
    time       <- arbitrary
    track      <- arbitrary
    return $ Stop id name coordinate routeIndex time track

instance Arbitrary StopLocation where
  arbitrary = do
    id    <- arbitrary
    name  <- arbitrary
    coord <- arbitrary
    return $ StopLocation id name coord

-- | Encode and Decode JSON and check whether the results match
encDec :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
encDec a = Right a == eitherDecode (encode a)

quickCheckTests = testGroup "Parsing and Serialization"
  [ QC.testProperty "serialize/deserialize connection" $
    \connection -> encDec (connection::Connection)
  , QC.testProperty "serialize/deserialize coordLocation" $
    \coordLocation -> encDec (coordLocation::CoordLocation)
  , QC.testProperty "serialize/deserialize journey" $
    \journey -> encDec (journey::Journey)
  , QC.testProperty "serialize/deserialize journeyRef" $
    \journeyRef -> encDec (journeyRef::JourneyRef)
  , QC.testProperty "serialize/deserialize journeyType" $
    \journeyType -> encDec (journeyType::JourneyType)
  , QC.testProperty "serialize/deserialie note" $
    \note -> encDec (note::Note)
  , QC.testProperty "serialize/deserialize operator" $
    \operator -> encDec (operator::Operator)
  , QC.testProperty "serialize/deserialize stop" $
    \stop -> encDec (stop::Stop)
  , QC.testProperty "serialize/deserialize stopLocation" $
    \stopLocation -> encDec (stopLocation::StopLocation)
  , QC.testProperty "serialize/deserialize transportType" $
    \transport -> encDec (transport::TransportType)
  ]

