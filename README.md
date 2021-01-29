# Oplog broadcast

## Pre-requistes
- Haskell Stack installed
- Elm, create-elm-app
- Mongo installed with a database called "murali"

## Initialize dataase

Using the `mongo` CLI tool, run the commands in the file `app/create_quote_coll.mongodb`
## Building

`stack build`

`cd elm-client; elm-app build`
## Running the server
In the root folder of the repository, run `stack run mongo-broadcast`

## Elm client

http://localhost:8080
## Running the Haskell CLI client

app/wsClient.hs

## Mongo updates

app/updateQuotesMongo.hs