{-# LANGUAGE OverloadedStrings #-}

module Integration (integrationTests) where

import           Data.Either                (isRight)
import           Data.Text                  (pack)
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.IO.Class     (liftIO)
import           Servant.Client
import           System.Environment         (getEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import Web.DeutscheBahn.API.Schedule.API
import Web.DeutscheBahn.API.Schedule.Data

apiAuthKey :: IO AuthKey
apiAuthKey = fmap (AuthKey . pack) (getEnv "DEUTSCHE_BAHN_AUTH_KEY")

queryDepartures :: IO (Either ServantError [Departure])
queryDepartures = do
  key <- apiAuthKey
  departureBoard Nothing key (StopId "008000105") (parseApiDate "2016-01-01") (parseApiTime "15:02")

queryArrivals :: IO (Either ServantError [Arrival])
queryArrivals = do
  key <- apiAuthKey
  arrivalBoard Nothing key (StopId "008000105") (parseApiDate "2016-01-01") (parseApiTime "15:02")

integrationTests = testGroup "Querying API"
  [ testCase "arrivalBoard" $ do
      arrivals <- queryArrivals
      assertBool "left" $ isRight arrivals
  , testCase "departureBoard" $ do
      departures <- queryDepartures
      assertBool "left" $ isRight departures
  , testCase "journeyDetails" $ do
      departures <- queryDepartures
      authKey    <- apiAuthKey
      let journeyRef = fmap (_departureJourneyRef . head) departures
          details    = fmap (journeyDetail Nothing authKey) journeyRef
      assertBool "left" $ isRight details
  ]
