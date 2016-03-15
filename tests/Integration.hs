{-# LANGUAGE OverloadedStrings #-}

module Integration (integrationTests) where

import           Data.Either                (isRight)
import           Data.Text                  (pack)
import           Data.Bifunctor             (bimap)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Data.Time.Format           (defaultTimeLocale, parseTimeOrError)
import           Data.Time.LocalTime        (LocalTime, TimeZone (..), hoursToTimeZone, minutesToTimeZone)
import           Control.Error.Safe         (headErr)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Control.Monad.IO.Class     (liftIO)
import           Servant.Client
import           System.Environment         (getEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import Web.DeutscheBahn.API.Schedule.API
import Web.DeutscheBahn.API.Schedule.Data

apiAuthKey :: IO AuthKey
apiAuthKey = return $ AuthKey  "DBhackFrankfurt0316"

-- | Winter UTC +1
germanyTimeZone :: TimeZone
germanyTimeZone = hoursToTimeZone 1

someTime :: LocalTime
someTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" "2016-04-06T12:00:00"

queryDepartures :: IO (Either ServantError [Departure])
queryDepartures = do
  key <- apiAuthKey
  departureBoard Nothing key (StopId "008000105") someTime

queryArrivals :: IO (Either ServantError [Arrival])
queryArrivals = do
  key <- apiAuthKey
  arrivalBoard Nothing key (StopId "008000105") someTime

integrationTests = testGroup "Querying API"
  [ testCase "locationName" $ do
      authKey <- apiAuthKey
      stops   <- locationName Nothing authKey "Frankfurt"
      assertBool (show stops) $ isRight stops
  , testCase "arrivalBoard" $ do
    arrivals <- queryArrivals
    assertBool (show arrivals) $ isRight arrivals
  , testCase "departureBoard" $ do
      departures <- queryDepartures
      assertBool (show departures) $ isRight departures
  , testCase "journeyDetails" $ do
      departures <- queryDepartures
      authKey    <- apiAuthKey
      let journeyRef = fmap (_departureJourneyRef . head) departures
          details    = fmap (journeyDetail Nothing authKey) journeyRef
      assertBool "left" $ isRight details
  , testCase "journeyDetails by stop and destination" $ do
     authKey    <- apiAuthKey
     utcNow     <- getCurrentTime
     stops      <- locationName Nothing authKey "Frankfurt"
     let stopId     = (fmap .fmap) _stopLocationId (headErr "Stop not found" <$> stops)
         departures = (fmap . fmap) (\s -> departureBoard Nothing authKey s someTime) stopId
     assertBool "left"$ isRight $ departures
  ]
