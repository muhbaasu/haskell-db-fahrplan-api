{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.DeutscheBahn.API.Schedule.API where

import           Data.Aeson
import           GHC.Generics
import           Data.Text                  (Text)
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

data DepartureBoardResponse = DepartureBoardResponse { _departure :: [Journey] } deriving (Show, Eq)

instance FromJSON DepartureBoardResponse where
  parseJSON (Object v) = DepartureBoardResponse <$>
                          ((v .: "DepartureBoard") >>= (.: "Departure"))

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

api :: Proxy DeutscheBahnAPI
api = Proxy

locationName :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe Text -> IO (Either ServantError LocationResponse)
locationName f l k i = runEitherT $ locationName_ f l k i

departureBoard :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe StopId -> Maybe Day -> Maybe TimeOfDay -> IO (Either ServantError DepartureBoardResponse)
departureBoard f l k s d t = runEitherT $ departureBoard_ f l k s d t

locationName_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe Text -> EitherT ServantError IO LocationResponse
departureBoard_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe StopId -> Maybe Day -> Maybe TimeOfDay -> EitherT ServantError IO DepartureBoardResponse
locationName_ :<|> departureBoard_ = client api (BaseUrl Http "open-api.bahn.de" 80)

