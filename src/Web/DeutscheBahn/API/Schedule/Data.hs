{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

-- | Type definitions for the Fahrplan API
module Web.DeutscheBahn.API.Schedule.Data where

import           Control.Lens.Getter            (view)
import           Data.Aeson
import           Data.Aeson.Types               (typeMismatch)
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text, unpack)
import           Data.Time.Calendar             (Day)
import           Data.Time.LocalTime            (LocalTime(..), TimeOfDay, TimeZone, hoursToTimeZone, localTimeToUTC)
import           Data.Time.Format               (defaultTimeLocale, formatTime, parseTimeOrError)
import           GHC.Generics                   (Generic)

newtype RouteIndex = RouteIndex {unRouteIndex :: Int} deriving (Eq, Show, Generic, ToJSON, FromJSON)
newtype StopId     = StopId     {unStopId     :: Text} deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | parse time formatted as e.g. 15:02
parseApiTime :: Text -> TimeOfDay
parseApiTime str = parseTimeOrError False defaultTimeLocale "%H:%M" (unpack str)

-- | parse time formatted as e.g. 2016-02-22
parseApiDate :: Text -> Day
parseApiDate str = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (unpack str)

-- | format time to e.g. 15:02
formatApiTime :: LocalTime -> String
formatApiTime t = formatTime defaultTimeLocale "%H:%M" t

-- | format date to e.g. 2016-02-22
formatApiDate :: LocalTime -> String
formatApiDate d = formatTime defaultTimeLocale "%Y-%m-%d" d

data StopCoordinate = StopCoordinate
  { _latitude :: Double
  , _longitude :: Double
  } deriving (Show, Eq)

data StopLocation = StopLocation
  { _stopLocationId           :: StopId
  , _stopLocationName         :: Text
  -- | combination of Latitude, Longitude
  , _stopLocationCoordinate   :: StopCoordinate
  } deriving (Show, Eq)

instance FromJSON StopLocation where
  parseJSON (Object v) = StopLocation <$>
                          (StopId <$> v .: "id") <*>
                          v .: "name" <*>
                          (StopCoordinate <$>
                             (read <$> v .: "lat") <*>
                             (read <$> v .: "lon"))
  parseJSON invalid    = typeMismatch "StopLocation" invalid

instance ToJSON StopLocation where
  toJSON a = object [ "id"   .= _stopLocationId a
                    , "name" .= _stopLocationName a
                    , "lat"  .= (show . _latitude . _stopLocationCoordinate) a
                    , "lon"  .= (show ._longitude . _stopLocationCoordinate) a
                    ]

data TransportType =
    ICE
  | IC
  | IRE
  | RE
  | SBahn
  deriving (Show, Eq)

instance FromJSON TransportType where
  parseJSON (String "ICE") = return ICE
  parseJSON (String "IC")  = return IC
  parseJSON (String "IRE") = return IRE
  parseJSON (String "RE")  = return RE
  parseJSON (String "S")   = return SBahn
  parseJSON invalid        = typeMismatch "TransportType" invalid

instance ToJSON TransportType where
  toJSON ICE   = "ICE"
  toJSON IC    = "IC"
  toJSON IRE   = "IRE"
  toJSON RE    = "RE"
  toJSON SBahn = "S"

-- | DepartureOrArrival
data Connection = Connection
  { _connectionName :: Text
  , _connectionTransportType :: TransportType
  , _connectionStopId        :: StopId
  -- | combination of date and time, time zone assumed to be GMT/UTC + 01:00
  , _connectionDateTime      :: LocalTime
  , _connectionStop          :: Text
  , _connectionDirection     :: Text
  , _connectionTrack         :: Text
  , _connectionJourneyRef    :: JourneyRef
  } deriving (Show, Eq)

instance FromJSON Connection where
  parseJSON (Object v) = Connection <$>
                          v .: "name" <*>
                          v .: "type" <*>
                          (StopId  <$> v .: "stopid") <*> -- API sends Int as String
                          (LocalTime <$>
                             (parseApiDate <$> v .: "date") <*>
                             (parseApiTime <$> v .: "time")) <*>
                          v .: "stop" <*>
                          v .: "direction" <*>
                          v .: "track" <*>
                          v .: "JourneyDetailRef"
  parseJSON invalid    = typeMismatch "Connection" invalid

instance ToJSON Connection where
  toJSON a = object [ "name"             .= _connectionName a
                    , "type"             .= _connectionTransportType a
                    , "stopid"           .= (unStopId . _connectionStopId) a
                    , "date"             .= (formatApiDate . _connectionDateTime) a
                    , "time"             .= (formatApiTime . _connectionDateTime) a
                    , "stop"             .= _connectionStop a
                    , "direction"        .= _connectionDirection a
                    , "track"            .= _connectionTrack a
                    , "JourneyDetailRef" .= _connectionJourneyRef a]

data JourneyRef = JourneyRef
  { _journeyRef :: Text
  } deriving (Show, Eq)

instance FromJSON JourneyRef where
  parseJSON (Object v) = JourneyRef <$> v .: "ref"
  parseJSON invalid    = typeMismatch "JourneyRef" invalid

instance ToJSON JourneyRef where
  toJSON a = object [ "ref" .= _journeyRef a]

data Journey = Journey
  { _journeyStops     :: [Stop]
  , _journeyNames     :: [Name]
  , _journeyTypes     :: [JourneyType]
  , _journeyOperators :: [Operator]
  , _journeyNotes     :: [Note]
  } deriving Show

instance FromJSON Journey where
  parseJSON (Object v) = Journey <$>
                          v .: "stops" <*>
                          v .: "names" <*>
                          v .: "types" <*>
                          v .: "operators" <*>
                          v .: "notes"
  parseJSON invalid    = typeMismatch "Journey" invalid

data Stop = Stop
  { _stopId            :: StopId
  , _stopName          :: Text
  -- | combination of Latitude, Longitude
  , _stopCoordinate    :: StopCoordinate
  , _stopRouteIndex    :: RouteIndex
  -- | combination of date and time
  , _stopDepartureTime :: LocalTime
  , _stopTrack         :: Text
  } deriving (Show, Eq)

instance FromJSON Stop where
  parseJSON (Object v) = Stop <$>
                         v .: "stop" <*>
                         v .: "name" <*>
                         (StopCoordinate <$> (v .: "lat") <*> (v .: "lon")) <*>
                         v .: "routeIdx" <*>
                         (LocalTime <$>
                            (parseApiDate <$> v .: "depDate") <*>
                            (parseApiTime <$> v .: "depTime")) <*>
                         v .: "track"
  parseJSON invalid    = typeMismatch "Stop" invalid

instance ToJSON Stop where
  toJSON a = object [ "stop" .= _stopId a
                    , "name" .= _stopName a
                    , "lat"  .= (_latitude . _stopCoordinate) a
                    , "lon"  .= (_longitude . _stopCoordinate) a
                    , "routeIdx" .= _stopRouteIndex a
                    , "depTime" .= (formatApiTime . _stopDepartureTime) a
                    , "depDate" .= (formatApiDate . _stopDepartureTime) a
                    , "track"   .= _stopTrack a
                    ]

data Name = Name
  { _nameName           :: Text
  , _nameRouteIndexFrom :: RouteIndex
  , _nameRouteIndexTo   :: RouteIndex
  } deriving Show

instance FromJSON Name where
  parseJSON (Object v) = Name <$>
                         v .: "name" <*>
                         (RouteIndex <$>  v .: "routeIdxFrom") <*>
                         (RouteIndex <$>  v .: "routeIdxTo")
  parseJSON invalid    = typeMismatch "Name" invalid

instance ToJSON Name where
  toJSON a = object [ "name"         .= _nameName a
                    , "routeIdxFrom" .= _nameRouteIndexFrom a
                    , "routeIdxTo"   .= _nameRouteIndexTo a]


data JourneyType = JourneyType
  { _journeyTypeTransportType  :: TransportType
  , _journeyTypeRouteIndexFrom :: RouteIndex
  , _journeyTypeRouteIndexTo   :: RouteIndex
  } deriving Show

instance FromJSON JourneyType where
  parseJSON (Object v) = JourneyType <$>
                         v .: "type" <*>
                         (RouteIndex <$>  v .: "routeIdxFrom") <*>
                         (RouteIndex <$>  v .: "routeIdxTo")
  parseJSON invalid    = typeMismatch "JourneyType" invalid

instance ToJSON JourneyType where
  toJSON a = object [ "name"         .= _journeyTypeTransportType a
                    , "routeIdxFrom" .= _journeyTypeRouteIndexFrom a
                    , "routeIdxTo"   .= _journeyTypeRouteIndexTo a]

data Operator = Operator
  { _operatorName :: Text
  , _operatorRouteIndexFrom :: RouteIndex
  , _operatorRouteIndexTo   :: RouteIndex
  } deriving Show

instance FromJSON Operator where
  parseJSON (Object v) = Operator <$>
                         v .: "name" <*>
                         (RouteIndex <$> v .: "routeIdxFrom") <*>
                         (RouteIndex <$> v .: "routeIdxTo")
  parseJSON invalid    = typeMismatch "Operator" invalid

instance ToJSON Operator where
  toJSON a = object [ "name"         .= _operatorName a
                    , "routeIdxFrom" .= _operatorRouteIndexFrom a
                    , "routeIdxTo"   .= _operatorRouteIndexTo a]

data Note = Note
  { _noteKey            :: Text
  , _notePriority       :: Int
  , _noteRouteIndexFrom :: RouteIndex
  , _noteRouteIndexTo   :: RouteIndex
  } deriving Show

instance FromJSON Note where
  parseJSON (Object v) = Note <$>
                         v .: "key" <*>
                         v .: "priority" <*>
                         (RouteIndex <$> v .: "routeIdxFrom") <*>
                         (RouteIndex <$> v .: "routeIdxTo")
  parseJSON invalid    = typeMismatch "Note" invalid

instance ToJSON Note where
  toJSON a = object [ "key"          .= _noteKey a
                    , "priority"     .= _notePriority a
                    , "routeIdxFrom" .= _noteRouteIndexFrom a
                    , "routeIdxTo"   .= _noteRouteIndexTo a]
