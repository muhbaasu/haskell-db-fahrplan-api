{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Unit (unitTests) where

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

import Web.DeutscheBahn.API.Schedule
import Web.DeutscheBahn.API.Schedule.Data

unitTests = testGroup "Unit Test Parsing"

  [
    testCase "parse JourneyDetailRef" $
      Right ref @=? (eitherDecode refJSON :: Either String JourneyRef)
  , testCase "parse Departure" $
      Right departure @=? (eitherDecode departureJSON :: Either String Departure)
  , testCase "parse StopLocation" $
      Right stopLocation @=? (eitherDecode stopLocationJSON :: Either String StopLocation)
  ]

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

departure :: Departure
departure = Departure "RE 15306" RE (StopId "8000105") departureTime "Frankfurt(Main)Hbf" "Limburg(Lahn)" "3" ref

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
