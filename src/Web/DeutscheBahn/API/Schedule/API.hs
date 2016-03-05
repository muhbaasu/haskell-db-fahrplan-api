{-# LANGUAGE DataKinds         #-}
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

newtype AuthKey = AuthKey {_unAuthKey :: Text} deriving (Show, Eq)

instance ToText AuthKey where
  toText = _unAuthKey

data JourneyDetailsResponse = JourneyDetailsResponse
  { _journeyDetail :: Journey} deriving (Show, Eq)

instance FromJSON JourneyDetailsResponse where
  parseJSON (Object v) = JourneyDetailsResponse <$> ( v .: "JourneyDetail")

type DeutscheBahnAPI =
  "bin/rest.exe/location.name"
    :> QueryParam "format"  ApiFormat
    :> QueryParam "lang"    ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "input"   Text
    :> Get '[JSON] LocationResponse
  :<|> "bin/rest.exe/departureBoard"
    :> QueryParam "format"  ApiFormat
    :> QueryParam "lang"    ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "id"      StopId
    :> QueryParam "date"    Day
    :> QueryParam "time"    TimeOfDay
    :> Get '[JSON] DepartureBoardResponse
  :<|> "bin/rest.exe/arrivalBoard"
    :> QueryParam "format"  ApiFormat
    :> QueryParam "lang"    ApiLanguage
    :> QueryParam "authKey" AuthKey
    :> QueryParam "id"      StopId
    :> QueryParam "date"    Day
    :> QueryParam "time"    TimeOfDay
    :> Get '[JSON] ArrivalBoardResponse
  :<|> "bin/rest.exe/journeyDetail"
    :> QueryParam "format"        ApiFormat
    :> QueryParam "lang"          ApiLanguage
    :> QueryParam "authKey"       AuthKey
    :> QueryParam "ref"           Ref
    :> QueryParam "date"          Day
    :> QueryParam "station_evaId" EvaId
    :> QueryParam "station_type"  Text
    :> Get '[JSON] JourneyDetailsResponse

api :: Proxy DeutscheBahnAPI
api = Proxy

apiFormat :: Maybe ApiFormat
apiFormat = Just FormatJSON

locationName :: Maybe ApiLanguage ->
                AuthKey ->
                Text ->
                IO (Either ServantError LocationList)
locationName l k i = runEitherT $ _locationList <$> locationName_ apiFormat lang key input
  where lang      = Just $ fromMaybe English l
        key       = Just k
        input     = Just i

departureBoard :: Maybe ApiLanguage ->
                  AuthKey ->
                  StopId ->
                  Day ->
                  TimeOfDay ->
                  IO (Either ServantError [Departure])
departureBoard l k s d t = runEitherT $ _departure <$>  departureBoard_ apiFormat lang key stop day time
  where lang      = Just $ fromMaybe English l
        key       = Just k
        stop      = Just s
        day       = Just d
        time      = Just t

arrivalBoard :: Maybe ApiLanguage ->
                AuthKey ->
                StopId ->
                Day ->
                TimeOfDay ->
                IO (Either ServantError [Arrival])
arrivalBoard l k s d t = runEitherT $ _arrival <$> arrivalBoard_ apiFormat lang key stop day time
  where lang      = Just $ fromMaybe English l
        key       = Just k
        stop      = Just s
        day       = Just d
        time      = Just t

journeyRef :: Maybe ApiLanguage ->
              AuthKey ->
              RefDetails ->
              IO (Either ServantError Journey)
journeyRef l k r = runEitherT $ _journeyDetail <$> journeyRef_ apiFormat lang key ref date evaId sType
  where lang      = Just $ fromMaybe English l
        key       = Just k
        ref       = Just $ _refDetailsRef r
        date      = Just $ _refDetailsDate r
        evaId     = Just $ _refDetailsEvaId r
        sType     = Just $ _refDetailsType r

locationName_   :: Maybe ApiFormat ->
                   Maybe ApiLanguage ->
                   Maybe AuthKey ->
                   Maybe Text ->
                   EitherT ServantError IO LocationResponse
departureBoard_ :: Maybe ApiFormat ->
                   Maybe ApiLanguage ->
                   Maybe AuthKey ->
                   Maybe StopId ->
                   Maybe Day ->
                   Maybe TimeOfDay ->
                   EitherT ServantError IO DepartureBoardResponse
arrivalBoard_   :: Maybe ApiFormat ->
                   Maybe ApiLanguage ->
                   Maybe AuthKey ->
                   Maybe StopId ->
                   Maybe Day ->
                   Maybe TimeOfDay ->
                   EitherT ServantError IO ArrivalBoardResponse
journeyRef_     :: Maybe ApiFormat ->
                   Maybe ApiLanguage ->
                   Maybe AuthKey ->
                   Maybe Ref ->
                   Maybe Day ->
                   Maybe EvaId ->
                   Maybe Text ->
                   EitherT ServantError IO JourneyDetailsResponse
locationName_
  :<|> departureBoard_
  :<|> arrivalBoard_
  :<|> journeyRef_ = client api (BaseUrl Http "open-api.bahn.de" 80)
