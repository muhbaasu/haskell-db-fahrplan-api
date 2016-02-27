-- | Type definitions for the Fahrplan API
module Web.DeutscheBahn.API.Schedule.Data
  ( -- * Types
    Connection
  , Journey
  , JourneyRef
  , JourneyType
  , Name
  , Note
  , Operator
  , Stop
  , StopLocation
  , TransportType
  ) where

import           Data.Geo.Coordinate.Coordinate (Coordinate)
import           Data.Text (Text)
import           Data.Time.Clock                (UTCTime)
import           Data.Time.LocalTime            ()

newtype RouteIndex = RouteIndex Int deriving (Eq, Show)
newtype StopId     = StopId Int     deriving (Eq, Show)

data StopLocation = StopLocation
  { _stopLocationId           :: StopId
  , _stopLocationName         :: Text
  -- | combination of Latitude, Longitude
  , _stopLocationCoordinate   :: Coordinate
  } deriving Show

data TransportType =
    ICE
  | IC
  | IRE
  | RE
  | S
  deriving Show

-- | DepartureOrArrival
data Connection = Connection
  { _connectionName :: Text
  , _connectionTransportType :: TransportType
  , _connectionStopId        :: StopId
  -- | combination of date and time, time zone assumed to be GMT/UTC + 01:00
  , _connectionDateTime      :: UTCTime
  , _connectionStop          :: Text
  , _connectionDirection     :: Text
  , _connectionTrack         :: Text
  , _connectionJourneyRef    :: JourneyRef
  } deriving Show

data JourneyRef = JourneyRef
  { _journeyRef :: Text
  } deriving Show

data Journey = Journey
  { _journeyStops     :: [Stop]
  , _journeyNames     :: [Name]
  , _journeyTypes     :: [JourneyType]
  , _journeyOperators :: [Operator]
  , _journeyNotes     :: [Note]
  } deriving Show

data Stop = Stop
  { _stopId            :: StopId
  , _stopName          :: Text
  , _stopCoordinate    :: Coordinate
  , _stopRouteIndex    :: RouteIndex
  -- | combination of date and time, time zone assumed to be GMT/UTC + 01:00
  , _stopDepartureTime :: UTCTime
  , _stopTrack         :: Text
  } deriving Show


data Name = Name
  { _nameName             :: Text
  , _nameRouteIndexFrom   :: RouteIndex
  , _nameRouteIndexTo     :: RouteIndex
  } deriving Show

data JourneyType = JourneyType
  { _typeTransportType     :: TransportType
  , _typeRouteIndexFrom    :: RouteIndex
  , _typeRouteIndexTo      :: RouteIndex
  } deriving Show

data Operator = Operator
  { _operatorName :: Text
  , _operatorRouteIndexFrom :: RouteIndex
  , _operatorRouteIndexTo   :: RouteIndex
  } deriving Show

data Note = Note
  { _noteKey            :: Text
  , _notePriority       :: Int
  , _noteRouteIndexFrom :: RouteIndex
  , _noteRouteIndexTo   :: RouteIndex
  } deriving Show

