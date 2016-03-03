# haskell-db-fahrplan-api
[![Build Status](https://travis-ci.org/muhbaasu/haskell-db-fahrplan-api.svg?branch=dev)](https://travis-ci.org/muhbaasu/haskell-db-fahrplan-api) [![Code Climate](https://codeclimate.com/github/muhbaasu/haskell-db-fahrplan-api/badges/gpa.svg)](https://codeclimate.com/github/muhbaasu/haskell-db-fahrplan-api)

High-level bindings to the db fahrplan api based on servant.

[API Definition](http://data.deutschebahn.com/apis/fahrplan)

High level types do not not exactly match the API definition.
- Date and time are combined to local time.
- Langitude and longitude are combined to coordinates.

