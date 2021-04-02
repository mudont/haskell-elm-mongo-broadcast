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
  [Enable Replicas](https://docs.mongodb.com/manual/tutorial/deploy-replica-set/_, because this app reads the Oplog. A single Mongo instance is enough. Don't need to actually replicate

## Initialize Mongo database

Using the `mongo` CLI tool, run the commands in the file `app/create_quote_coll.mongodb`

## Config

Edit `src/Config.hs`

Edit the websocket url in `elm-client/src/index.js`

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

```
stack run ws-client
```
## Update Mongo database with some fake data for about 30 seconds
```
stack run update-quotes-mongo
```
## tail mongo oplog and print raw oplog documents to console
```
stack run tail-oplog
```

You should see the quotes ticking in the Elm client and the CLI client writing them to the console 

## Known Issues
- Assumes default Mongo port, hardcodes HTTP port 8080
- Hardcodes db/collection names

## Wishlist
- don't hardcode ports, db collections
- More realistic quote objects with 
- Subscriptions
- throttling. May want different rates for different subscriptions
