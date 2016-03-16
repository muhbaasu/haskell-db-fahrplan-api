{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.DeutscheBahn.API.Schedule.API where

import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Text                  (Text)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy
import           Data.Time.Calendar         (Day)
import           Data.Time.LocalTime        (LocalTime (..), TimeOfDay, localDay, localTimeOfDay)
import           GHC.Generics
import           Servant.API
import           Servant.Client

import Web.DeutscheBahn.API.Schedule.Data
import Web.DeutscheBahn.API.Schedule.JourneyRefURIParser

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
                EitherT ServantError IO [StopLocation]
locationName l k i = (_stopLocation . _locationList) <$> locationName_ apiFormat lang key input
  where lang      = Just $ fromMaybe English l
        key       = Just k
        input     = Just i

departureBoard :: Maybe ApiLanguage ->
                  AuthKey ->
                  StopId ->
                  LocalTime ->
                  EitherT ServantError IO [Departure]
departureBoard l k s lt = _departure <$>  departureBoard_ apiFormat lang key stop day time
  where lang      = Just $ fromMaybe English l
        key       = Just k
        stop      = Just s
        day       = Just $ localDay lt
        time      = Just $ localTimeOfDay lt

arrivalBoard :: Maybe ApiLanguage ->
                AuthKey ->
                StopId ->
                LocalTime ->
                EitherT ServantError IO [Arrival]
arrivalBoard l k s lt = _arrival <$> arrivalBoard_ apiFormat lang key stop day time
  where lang      = Just $ fromMaybe English l
        key       = Just k
        stop      = Just s
        day       = Just $ localDay lt
        time      = Just $ localTimeOfDay lt

journeyDetail :: Maybe ApiLanguage ->
                 AuthKey ->
                 JourneyRef ->
                 EitherT ServantError IO Journey
journeyDetail l k r = _journeyDetail <$> journeyDetail_ apiFormat lang key ref date evaId sType
  where lang       = Just $ fromMaybe English l
        key        = Just k
        refDetails = rightToMaybe <$> parseJourneyRefURI $ _journeyRef r
        ref        = _refDetailsRef   <$> refDetails
        date       = _refDetailsDate  <$> refDetails
        evaId      = _refDetailsEvaId <$> refDetails
        sType      = _refDetailsType  <$> refDetails

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
journeyDetail_  :: Maybe ApiFormat ->
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
  :<|> journeyDetail_ = client api (BaseUrl Http "open-api.bahn.de" 80)
