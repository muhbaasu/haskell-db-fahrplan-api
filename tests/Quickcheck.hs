{-# LANGUAGE OverloadedStrings #-}

module Quickcheck (quickCheckTests) where

import Data.Aeson
import Data.Time.Calendar                  (Day(..))
import Data.Time.Clock                     (UTCTime)
import Data.Text                           (Text, pack)
import Data.Time.ISO8601                   (parseISO8601)
import Data.Time.LocalTime                 (LocalTime(..), TimeZone, TimeOfDay(..), hoursToTimeZone, utcToLocalTime)
import Test.Tasty
import Test.Tasty.QuickCheck               as QC

import Web.DeutscheBahn.API.Schedule
import Web.DeutscheBahn.API.Schedule.Data

quickCheckTests = testGroup "QuickCheck Tests"
  [ QC.testProperty "serialize/deserialize arrival" $
    \arrival -> encDec (arrival::Arrival)
  , QC.testProperty "serialize/deserialize departure" $
    \departure -> encDec (departure::Departure)
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

-- | Encode and Decode JSON and check whether the results match
encDec :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
encDec a = Right a == eitherDecode (encode a)

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

instance Arbitrary Arrival where
  arbitrary = do
    depName       <- arbitrary
    transportType <- arbitrary
    stopId        <- arbitrary
    time          <- arbitrary
    stop          <- arbitrary
    origin        <- arbitrary
    track         <- arbitrary
    ref           <- arbitrary
    return $ Arrival depName transportType stopId time stop origin track ref

instance Arbitrary Departure where
  arbitrary = do
    depName       <- arbitrary
    transportType <- arbitrary
    stopId        <- arbitrary
    time          <- arbitrary
    stop          <- arbitrary
    dir           <- arbitrary
    track         <- arbitrary
    ref           <- arbitrary
    return $ Departure depName transportType stopId time stop dir track ref

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
    opName          <- arbitrary
    routeIdxFrom  <- arbitrary
    routeIdxTo    <- arbitrary
    return $ Operator opName routeIdxFrom routeIdxTo

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

