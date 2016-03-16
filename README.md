# haskell-db-fahrplan-api
[![Build Status](https://travis-ci.org/muhbaasu/haskell-db-fahrplan-api.svg?branch=dev)](https://travis-ci.org/muhbaasu/haskell-db-fahrplan-api) [![Code Climate](https://codeclimate.com/github/muhbaasu/haskell-db-fahrplan-api/badges/gpa.svg)](https://codeclimate.com/github/muhbaasu/haskell-db-fahrplan-api)

High-level bindings to the db fahrplan api based on servant.

[API Definition](http://data.deutschebahn.com/apis/fahrplan)

Note, there are currently multiple open issues related to the API,
some of them making the API unreliable.
E.g. currently single array JSON results are returned as objects.
Without a lot of effort there are no workarounds for problems like this.
See [db-fahrplan-api issues](https://github.com/dbopendata/db-fahrplan-api/issues) for details.


## Integration Test Setup
To run the integration tests locally set the environment variable
`DEUTSCHE_BAHN_AUTH_KEY` to the key you received during the API registration process.
E.g. `export DEUTSCHE_BAHN_AUTH_KEY=insert_key`

