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

data LocationResponse = LocationResponse { _locationList :: LocationList } deriving (Show, Eq, Generic)

instance FromJSON LocationResponse where
  parseJSON (Object v) = LocationResponse <$> v .: "LocationList"

newtype AuthKey = AuthKey {_unAuthKey :: Text} deriving (Show, Eq)

instance ToText AuthKey where
  toText key = _unAuthKey key

type DeutscheBahnAPI =  "bin/rest.exe/location.name"
  :> QueryParam "format" ApiFormat
  :> QueryParam "lang" ApiLanguage
  :> QueryParam "authKey" AuthKey
  :> QueryParam "input" Text
  :> Get '[JSON] LocationResponse

api :: Proxy DeutscheBahnAPI
api = Proxy

locationName :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe Text -> IO (Either ServantError LocationResponse)
locationName f l k i = runEitherT $ locationName_ f l k i

locationName_ :: Maybe ApiFormat -> Maybe ApiLanguage -> Maybe AuthKey -> Maybe Text -> EitherT ServantError IO LocationResponse
locationName_ = client api (BaseUrl Http "open-api.bahn.de" 80)

