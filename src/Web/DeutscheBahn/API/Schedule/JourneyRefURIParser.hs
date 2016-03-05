{-# LANGUAGE OverloadedStrings #-}

module Web.DeutscheBahn.API.Schedule.JourneyRefURIParser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text                      (Text, unpack, pack)
import           Network.URI                    (URI, parseURI, unEscapeString)
import           Network.HTTP.Types.URI         (parseQuery)

import Web.DeutscheBahn.API.Schedule.Data

-- | Parse invalid formatted JourneyRef URI
parseJourneyRefURI  :: Text -> Either String RefDetails
parseJourneyRefURI uri = let unescaped = pack $ unEscapeString $ unpack uri
                 in eitherResult $ parse journeyRefUriParser unescaped

journeyRefUriParser :: Parser RefDetails
journeyRefUriParser = do
  _     <- "http://open-api.bahn.de/bin/rest.exe/v1.0/journeyDetail?ref=" <?> "prefix"
  ref   <- takeTill (== '?') <?> "ref"
  date  <- "?date=" *> takeTill (== '&') <?> "date"
  evaId <- "&station_evaId=" *> takeTill (== '&') <?> "evaId"
  sType <- "&station_type=" *> takeTill (== '&') <?> "stationType"
  return $ RefDetails (parseApiDate date) (Ref ref) (EvaId evaId) sType
