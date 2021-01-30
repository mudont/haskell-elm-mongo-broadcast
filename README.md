# Oplog broadcast

- Haskell Server tails Mongo oplog and broadcast changes to the collection `quotes.quote` and broadcast them to all subscribed clients. Quotes are dead trivial and contain just a symbol and a price.
- An Elm SPA client as well as a Haskell CLI client.
- A haskell script to repeatedly update a handful of fake securities with fake prices.


## Pre-requistes

If you don't have these already, install:
- [Stack](https://docs.haskellstack.org/en/stable/README/):\
   ```curl -sSL https://get.haskellstack.org/ | sh```
- [Elm](https://elm-lang.org/):
- [create-elm-app](https://github.com/halfzebra/create-elm-app):\
  ```npm install create-elm-app -g```
- [MongoDB](https://docs.mongodb.com/manual/installation/) (version 4.4 known to work)

## Initialize Mongo database

Using the `mongo` CLI tool, run the commands in the file `app/create_quote_coll.mongodb`
## Building

:warning: Stack could take a really long time if running for first time.

Clone this repo, and from the top level directory of this repo, run:
```
stack build
cd elm-client; elm-app build
```
## Running the server
In the root folder of the repository, run 
```
stack run ws-server
```

## Open Elm client

http://localhost:8080
## Running the Haskell CLI client

You can run in interpreted mode:
```
app/wsClient.hs
```

or run the compiled version:
```
stack run ws-client
```

## Update Mongo database with some fake data
```
app/updateQuotesMongo.hs
```
## tail mongo oplog and show raw oplog documents
```
app/tailOplog.hs
```

You should see the quotes ticking in the Elm client and the CLI client writing them to the console 

## Known Issues
- Assumes default Mongo port, hardcodes HTTP port 8080
- Hardcodes db/collection names
