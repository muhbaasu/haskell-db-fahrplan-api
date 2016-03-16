{-# LANGUAGE OverloadedStrings #-}

module Web.DeutscheBahn.API.Schedule.Response where

import           Data.Aeson
import           Data.Text                  (Text)

import Web.DeutscheBahn.API.Schedule.Data

data JourneyDetailsResponse = JourneyDetailsResponse
  { _journeyDetail :: Journey} deriving (Show, Eq)

instance FromJSON JourneyDetailsResponse where
  parseJSON (Object v) = JourneyDetailsResponse <$> ( v .: "JourneyDetail")
data LocationResponse = LocationResponse
  { _locationList :: LocationList
  } deriving (Show, Eq)

instance FromJSON LocationResponse where
  parseJSON (Object v) = LocationResponse <$> v .: "LocationList"

data DepartureBoardResponse = DepartureBoardResponse
  { _departure :: [Departure] } deriving (Show, Eq)

instance FromJSON DepartureBoardResponse where
  parseJSON (Object v) = DepartureBoardResponse <$>
                          ((v .: "DepartureBoard") >>= (.: "Departure"))

data ArrivalBoardResponse = ArrivalBoardResponse
  { _arrival :: [Arrival]} deriving (Show, Eq)

instance FromJSON ArrivalBoardResponse where
  parseJSON (Object v) = ArrivalBoardResponse <$>
                          ((v .: "ArrivalBoard") >>= (.: "Arrival"))
