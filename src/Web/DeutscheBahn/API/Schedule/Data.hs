{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

-- | Type definitions for the Fahrplan API
module Web.DeutscheBahn.API.Schedule.Data where

import           Control.Lens.Getter            (view)
import           Control.Lens.TH                (makeLenses)
import           Data.Aeson
import           Data.Aeson.Types               (typeMismatch)
import qualified Data.ByteString                as BS
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text, unpack, pack)
import           Data.Time.Calendar             (Day)
import           Data.Time.LocalTime            (LocalTime(..), TimeOfDay, TimeZone, hoursToTimeZone, localTimeToUTC)
import           Data.Time.Format               (FormatTime, defaultTimeLocale, formatTime, parseTimeOrError)
import           GHC.Generics                   (Generic)
import           Servant.API                    (ToText(..))

newtype RouteIndex = RouteIndex {unRouteIndex :: Int} deriving (Eq, Show, Generic, ToJSON, FromJSON)
newtype StopId     = StopId     {unStopId     :: Text} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToText StopId where
  toText = unStopId

-- | parse time formatted as e.g. 15:02
parseApiTime :: Text -> TimeOfDay
parseApiTime str = parseTimeOrError False defaultTimeLocale "%H:%M" (unpack str)

-- | parse time formatted as e.g. 2016-02-22
parseApiDate :: Text -> Day
parseApiDate str = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (unpack str)

-- | format time to e.g. 15:02
formatApiTime :: FormatTime t => t -> String
formatApiTime = formatTime defaultTimeLocale "%H:%M"

-- | format date to e.g. 2016-02-22
formatApiDate :: FormatTime t => t -> String
formatApiDate = formatTime defaultTimeLocale "%Y-%m-%d"

newtype Ref = Ref {unRef :: Text} deriving (Eq, Show, ToText)

newtype EvaId = EvaId {unEvaId :: Text} deriving (Eq, Show, ToText)

data RefDetails = RefDetails
  { _refDetailsDate  :: Day
  , _refDetailsRef   :: Ref
  , _refDetailsEvaId :: EvaId
  , _refDetailsType  :: Text
  } deriving (Show, Eq)

makeLenses ''RefDetails

instance ToText TimeOfDay where
  toText t = pack $ formatApiTime t

instance ToText Day where
  toText d = pack $ formatApiDate d

data LocationList = LocationList
  { _stopLocation  :: [StopLocation]
  } deriving (Show, Eq, Generic)

instance FromJSON LocationList where
  parseJSON (Object v) = LocationList <$>
                          v .: "StopLocation"

data Coordinate = Coordinate
  { _latitude :: Double
  , _longitude :: Double
  } deriving (Show, Eq)

data StopLocation = StopLocation
  { _stopLocationId           :: StopId
  , _stopLocationName         :: Text
  -- | combination of Latitude, Longitude
  , _stopLocationCoordinate   :: Coordinate
  } deriving (Show, Eq)

makeLenses ''StopLocation

instance FromJSON StopLocation where
  parseJSON (Object v) = StopLocation <$>
                          (StopId <$> v .: "id") <*>
                          v .: "name" <*>
                          (Coordinate <$>
                             (read <$> v .: "lat") <*>
                             (read <$> v .: "lon"))
  parseJSON invalid    = typeMismatch "StopLocation" invalid

instance ToJSON StopLocation where
  toJSON a = object [ "id"   .= _stopLocationId a
                    , "name" .= _stopLocationName a
                    , "lat"  .= (show . _latitude . _stopLocationCoordinate) a
                    , "lon"  .= (show . _longitude . _stopLocationCoordinate) a
                    ]

data TransportType =
    ICE
  | IC
  | IRE
  | RE
  | SBahn
  | UnknownTransport Text
  deriving (Show, Eq)

instance FromJSON TransportType where
  parseJSON (String "ICE")    = return ICE
  parseJSON (String "IC")     = return IC
  parseJSON (String "IRE")    = return IRE
  parseJSON (String "RE")     = return RE
  parseJSON (String "S")      = return SBahn
  parseJSON (String unknown)  = return $ UnknownTransport unknown
  parseJSON invalid           = typeMismatch "TransportType" invalid

instance ToJSON TransportType where
  toJSON ICE                  = "ICE"
  toJSON IC                   = "IC"
  toJSON IRE                  = "IRE"
  toJSON RE                   = "RE"
  toJSON SBahn                = "S"
  toJSON (UnknownTransport t) = String t

toTransportType :: Text -> TransportType
toTransportType "ICE"   = ICE
toTransportType "IC"    = IC
toTransportType "IRE"   = IRE
toTransportType "RE"    = RE
toTransportType "S"     = SBahn
toTransportType unknown = UnknownTransport unknown

data Stop = Stop
  { _stopId            :: StopId
  , _stopName          :: Text
  -- | combination of Latitude, Longitude
  , _stopCoordinate    :: Coordinate
  , _stopRouteIndex    :: RouteIndex
  -- | combination of date and time
  , _stopDepartureTime :: LocalTime
  , _stopTrack         :: Text
  } deriving (Show, Eq)

makeLenses ''Stop

instance FromJSON Stop where
  parseJSON (Object v) = Stop <$>
                         v .: "stop" <*>
                         v .: "name" <*>
                         (Coordinate <$> (v .: "lat") <*> (v .: "lon")) <*>
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
  } deriving (Show, Eq)

makeLenses ''Name

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

data JourneyRef = JourneyRef
  { _journeyRef :: Text
  } deriving (Show, Eq)

instance FromJSON JourneyRef where
  parseJSON (Object v) = JourneyRef <$> (v .: "ref")
  parseJSON invalid    = typeMismatch "JourneyRef" invalid

instance ToJSON JourneyRef where
  toJSON a = object [ "ref" .= _journeyRef a ]

instance ToText JourneyRef where
  toText j = pack . show $ _journeyRef j

data JourneyType = JourneyType
  { _journeyTypeTransportType  :: TransportType
  , _journeyTypeRouteIndexFrom :: RouteIndex
  , _journeyTypeRouteIndexTo   :: RouteIndex
  } deriving (Show, Eq)

makeLenses ''JourneyType

instance FromJSON JourneyType where
  parseJSON (Object v) = JourneyType <$>
                         (toTransportType <$> v .: "type") <*>
                         (RouteIndex <$> v .: "routeIdxFrom") <*>
                         (RouteIndex <$> v .: "routeIdxTo")
  parseJSON invalid    = typeMismatch "JourneyType" invalid

instance ToJSON JourneyType where
  toJSON a = object [ "type"         .= _journeyTypeTransportType a
                    , "routeIdxFrom" .= _journeyTypeRouteIndexFrom a
                    , "routeIdxTo"   .= _journeyTypeRouteIndexTo a]

data Operator = Operator
  { _operatorName :: Text
  , _operatorRouteIndexFrom :: RouteIndex
  , _operatorRouteIndexTo   :: RouteIndex
  } deriving (Show, Eq)

makeLenses ''Operator

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
  } deriving (Show, Eq)

makeLenses ''Note

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

data Arrival = Arrival
  { _arrivalName :: Text
  , _arrivalTransportType :: TransportType
  , _arrivalStopId        :: StopId
  -- | combination of date and time, time zone assumed to be GMT/UTC + 01:00
  , _arrivalDateTime      :: LocalTime
  , _arrivalStop          :: Text
  , _arrivalOrigin        :: Text
  , _arrivalTrack         :: Text
  , _arrivalJourneyRef    :: JourneyRef
  } deriving (Show, Eq)

makeLenses ''Arrival

instance FromJSON Arrival where
  parseJSON (Object v) = Arrival <$>
                          v .: "name" <*>
                          (toTransportType <$> v .: "type") <*>
                          (StopId  <$> v .: "stopid") <*> -- API sends Int as String
                          (LocalTime <$>
                             (parseApiDate <$> v .: "date") <*>
                             (parseApiTime <$> v .: "time")) <*>
                          v .: "stop" <*>
                          v .: "origin" <*>
                          v .: "track" <*>
                          v .: "JourneyDetailRef"
  parseJSON invalid    = typeMismatch "Arrival" invalid

instance ToJSON Arrival where
  toJSON a = object [ "name"             .= _arrivalName a
                    , "type"             .= _arrivalTransportType a
                    , "stopid"           .= (unStopId . _arrivalStopId) a
                    , "date"             .= (formatApiDate . _arrivalDateTime) a
                    , "time"             .= (formatApiTime . _arrivalDateTime) a
                    , "stop"             .= _arrivalStop a
                    , "origin"           .= _arrivalOrigin a
                    , "track"            .= _arrivalTrack a
                    , "JourneyDetailRef" .= _arrivalJourneyRef a]

data Departure = Departure
  { _departureName :: Text
  , _departureTransportType :: TransportType
  , _departureStopId        :: StopId
  -- | combination of date and time, time zone assumed to be GMT/UTC + 01:00
  , _departureDateTime      :: LocalTime
  , _departureStop          :: Text
  , _departureDirection     :: Text
  , _departureTrack         :: Text
  , _departureJourneyRef    :: JourneyRef
  } deriving (Show, Eq)

makeLenses ''Departure

instance FromJSON Departure where
  parseJSON (Object v) = Departure <$>
                          v .: "name" <*>
                          (toTransportType <$> v .: "type") <*>
                          (StopId  <$> v .: "stopid") <*>
                          (LocalTime <$>
                             (parseApiDate <$> v .: "date") <*>
                             (parseApiTime <$> v .: "time")) <*>
                          v .: "stop" <*>
                          v .: "direction" <*>
                          v .: "track" <*>
                          v .: "JourneyDetailRef"
  parseJSON invalid    = typeMismatch "Arrival" invalid

instance ToJSON Departure where
  toJSON a = object [ "name"             .= _departureName a
                    , "type"             .= _departureTransportType a
                    , "stopid"           .= (unStopId . _departureStopId) a
                    , "date"             .= (formatApiDate . _departureDateTime) a
                    , "time"             .= (formatApiTime . _departureDateTime) a
                    , "stop"             .= _departureStop a
                    , "direction"        .= _departureDirection a
                    , "track"            .= _departureTrack a
                    , "JourneyDetailRef" .= _departureJourneyRef a]

data Journey = Journey
  { _journeyStops     :: [Stop]
  , _journeyNames     :: [Name]
  , _journeyTypes     :: [JourneyType]
  , _journeyOperators :: [Operator]
  , _journeyNotes     :: [Note]
  } deriving (Show, Eq)

makeLenses ''Journey

instance FromJSON Journey where
  parseJSON (Object v) = Journey <$>
                          ((v .: "stops")     >>= (.: "stop")) <*>
                          ((v .: "names")     >>= (.: "name")) <*>
                          ((v .: "types")     >>= (.: "type")) <*>
                          ((v .: "operators") >>= (.: "operator")) <*>
                          ((v .: "notes")     >>= (.: "note"))
  parseJSON invalid    = typeMismatch "Journey" invalid

instance ToJSON Journey where
  toJSON a = object [ "stops"     .= object ["stop"     .= toJSON (_journeyStops a)]
                    , "names"     .= object ["name"     .= toJSON (_journeyNames a)]
                    , "types"     .= object ["type"     .= toJSON (_journeyTypes a)]
                    , "operators" .= object ["operator" .= toJSON (_journeyOperators a)]
                    , "notes"     .= object ["note"     .= toJSON (_journeyNotes a)]]
