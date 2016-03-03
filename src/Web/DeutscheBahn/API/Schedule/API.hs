{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.DeutscheBahn.API.Schedule.API where

import           Data.Aeson
import           GHC.Generics
import           Data.Text                  (Text)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy
import           Data.Time.Calendar         (Day)
import           Data.Time.LocalTime        (TimeOfDay)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Servant.API
import           Servant.Client

import Web.DeutscheBahn.API.Schedule.Data

data ApiFormat = FormatJSON | FormatXML

instance Show ApiFormat where
  show FormatJSON = "json"
  show FormatXML  = "xml"

instance ToText ApiFormat where
  toText FormatJSON = "json"
  toText FormatXML = "xml"

data ApiLanguage = English | German

instance Show ApiLanguage where
  show English = "english"
  show German  = "german"

instance ToText ApiLanguage where
  toText English = "english"
  toText German  = "german"

data LocationResponse = LocationResponse { _locationList :: LocationList } deriving (Show, Eq)

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

newtype AuthKey = AuthKey {_unAuthKey :: Text} deriving (Show, Eq)

instance ToText AuthKey where
  toText key = _unAuthKey key

type DeutscheBahnAPI =
  "bin/rest.exe/location.name"
    :> QueryParam "format"  ApiFormat
    :> QueryParam "lang"    ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "input"   Text
    :> Get '[JSON] LocationResponse
  :<|> "bin/rest.exe/departureBoard"
    :> QueryParam "format" ApiFormat
    :> QueryParam "lang"   ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "id"     StopId
    :> QueryParam "date"   Day
    :> QueryParam "time"   TimeOfDay
    :> Get '[JSON] DepartureBoardResponse
  :<|> "bin/rest.exe/arrivalBoard"
    :> QueryParam "format" ApiFormat
    :> QueryParam "lang"   ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "id"     StopId
    :> QueryParam "date"   Day
    :> QueryParam "time"   TimeOfDay
    :> Get '[JSON] ArrivalBoardResponse

api :: Proxy DeutscheBahnAPI
api = Proxy

locationName :: Maybe ApiLanguage ->AuthKey -> Text -> IO (Either ServantError LocationList)
locationName l k i = runEitherT $ _locationList <$> locationName_ format lang key input
  where format = Just FormatJSON
        lang   = Just $ fromMaybe English l
        key    = Just k
        input  = Just i

departureBoard :: Maybe ApiLanguage -> AuthKey -> StopId -> Day -> TimeOfDay -> IO (Either ServantError [Departure])
departureBoard l k s d t = runEitherT $ _departure <$>  departureBoard_ format lang key stop day time
  where format = Just FormatJSON
        lang   = Just $ fromMaybe English l
        key    = Just k
        stop   = Just s
        day    = Just d
        time   = Just t

arrivalBoard :: Maybe ApiLanguage -> AuthKey -> StopId -> Day -> TimeOfDay -> IO (Either ServantError [Arrival])
arrivalBoard l k s d t = runEitherT $ _arrival <$> arrivalBoard_ format lang key stop day time
  where format = Just FormatJSON
        lang   = Just $ fromMaybe English l
        key    = Just k
        stop   = Just s
        day    = Just d
        time   = Just t

locationName_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe Text -> EitherT ServantError IO LocationResponse
departureBoard_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe StopId -> Maybe Day -> Maybe TimeOfDay -> EitherT ServantError IO DepartureBoardResponse
arrivalBoard_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe StopId -> Maybe Day -> Maybe TimeOfDay -> EitherT ServantError IO ArrivalBoardResponse
locationName_ :<|> departureBoard_ :<|> arrivalBoard_ = client api (BaseUrl Http "open-api.bahn.de" 80)

