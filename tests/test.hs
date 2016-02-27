{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text                           (Text)
import Data.Text.Encoding                  (encodeUtf8)
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
  "ref":"ref"
  }
  }|]

ref :: ByteString
ref = fromStrict $ encodeUtf8 [text|
  {
  "ref":"http://DOMAINE-TOBE-DEFI-NED.de/bin/"
  }|]

unitTests = testGroup "Parsing"

  [
    testCase "parse JourneyDetailRef" $
        Right "http://DOMAINE-TOBE-DEFI-NED.de/bin/" @=? (_journeyRef <$> (eitherDecode ref :: Either String JourneyRef ))
  , testCase "parse Departure" $
      Right "RE 15306" @=? (_connectionName <$> (eitherDecode departureJSON :: Either String Connection))
  ]

