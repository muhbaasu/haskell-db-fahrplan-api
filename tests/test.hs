{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy
import Data.Maybe                          (fromJust)
import Data.Text                           (Text)
import Data.Text.Encoding                  (encodeUtf8)
import Data.Time.Clock                     (UTCTime)
import Data.Time.ISO8601                   (parseISO8601)
import Data.Time.LocalTime                 (LocalTime, TimeZone, hoursToTimeZone, utcToLocalTime)
import NeatInterpolation
import Test.Tasty
import Test.Tasty.HUnit

import Web.DeutscheBahn.API.Schedule
import Web.DeutscheBahn.API.Schedule.Data

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

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

germanyTimeZone :: TimeZone
germanyTimeZone = hoursToTimeZone 1 -- +1 UTC non-summer

departureTime :: LocalTime
departureTime = utcToLocalTime germanyTimeZone departureUTC
  where departureUTC = fromJust $ parseISO8601 "2016-02-22T14:01:00Z"

departure :: Connection
departure = Connection "RE 15306" RE (StopId 8000105) departureTime "Frankfurt(Main)Hbf" "Limburg(Lahn)" "3" ref

refJSON :: ByteString
refJSON = fromStrict $ encodeUtf8 [text|
  {
  "ref":"http://DOMAINE-TOBE-DEFI-NED.de/bin/"
  }|]

ref :: JourneyRef
ref = JourneyRef "http://DOMAINE-TOBE-DEFI-NED.de/bin/"

unitTests = testGroup "Parsing"

  [
    testCase "parse JourneyDetailRef" $
        Right ref @=? (eitherDecode refJSON :: Either String JourneyRef )
  , testCase "parse Departure" $
      Right departure @=? (eitherDecode departureJSON :: Either String Connection)
  ]

