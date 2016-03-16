{-# LANGUAGE OverloadedStrings #-}

module Integration (integrationTests, integrationTestsBrokenAPI) where

import           Data.Either                (isRight, isLeft)
import           Data.Text                  (pack)
import           Data.Bifunctor             (bimap)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Data.Time.Format           (defaultTimeLocale, parseTimeOrError)
import           Data.Time.LocalTime        (LocalTime, TimeZone (..), hoursToTimeZone, minutesToTimeZone)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Control.Monad.IO.Class     (liftIO)
import           Servant.Client
import           System.Environment         (getEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import Web.DeutscheBahn.API.Schedule.API
import Web.DeutscheBahn.API.Schedule.Data

apiAuthKey :: IO AuthKey
apiAuthKey = fmap (AuthKey . pack) (getEnv "DEUTSCHE_BAHN_AUTH_KEY")

-- | Winter UTC +1
germanyTimeZone :: TimeZone
germanyTimeZone = hoursToTimeZone 1

someTime :: LocalTime
someTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" "2016-04-06T12:00:00"

integrationTests = testGroup "Querying API"
  [ testCase "locationName" $ do
      authKey <- apiAuthKey
      locations' <- runEitherT $ locationName Nothing authKey "Frankfurt"
      assertBool (show locations') $ isRight locations'
  , testCase "arrivalBoard" $ do
      authKey <- apiAuthKey
      arrivals' <- runEitherT $ arrivalBoard Nothing authKey (StopId "008000105") someTime
      assertBool (show arrivals') $ isRight arrivals'
  , testCase "departureBoard" $ do
      authKey <- apiAuthKey
      departures' <- runEitherT $ departureBoard Nothing authKey (StopId "008000105") someTime
      assertBool (show departures') $ isRight departures'
  ]

-- | Currently failing tests due to improper implemented API
-- | see https://github.com/dbopendata/db-fahrplan-api/issues
integrationTestsBrokenAPI = testGroup "Currently broken API"
  [
    testCase "journeyDetails" $ do
        authKey    <- apiAuthKey
        details    <- runEitherT $ do
           let depQuery = departureBoard Nothing authKey (StopId "008000105") someTime
           journeyRef' <- (_departureJourneyRef . head) <$> depQuery
           journeyDetail Nothing authKey journeyRef'
        -- currently likely broken due to API not accepting proper formatted journeyRef urls
        assertBool (show details) $ isRight details
    , testCase "journeyDetails via location and departures" $ do
       authKey     <- apiAuthKey
       utcNow      <- getCurrentTime
       detail' <- runEitherT $ do
         stopId'     <- (_stopLocationId . head) <$> locationName Nothing authKey "Leipzig"
         journeyRef' <- (_departureJourneyRef . head) <$> departureBoard Nothing authKey stopId' someTime
         journeyDetail Nothing authKey journeyRef'
       -- currently likely broken due to API not accepting proper formatted journeyRef urls
       assertBool (show detail') $ isRight detail'
  ]
