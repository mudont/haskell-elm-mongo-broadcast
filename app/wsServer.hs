#!/usr/bin/env stack
-- stack script --resolver lts-17.0

-- websockets example
-- ==================
-- This is the Haskell implementation of the example for the WebSockets library. We
-- implement a simple multi-user chat program. A live demo of the example is
-- available [here](/example/client.html).  In order to understand this example,
-- keep the [reference](/reference/) nearby to check out the functions we use.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import Prelude

-- import App
import ClassyPrelude
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.MongoDB
import Network.Wai (Application)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Oplog
import Prelude (read)

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.

type Client = (Text, WS.Connection)

-- The state kept on the server is simply a list of connected clients. We've added
-- an alias and some utility functions, so it will be easier to extend this state
-- later on.

type ServerState = [Client]

-- Create a new, initial state:

newServerState :: ServerState
newServerState = []

-- Get the number of active clients:

numClients :: ServerState -> Int
numClients = length

-- Check if a user already exists (based on username):

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- Remove a client:

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Send a message to all clients, and log it on stdout:

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastQuote :: Quote -> MVar ServerState -> IO ()
broadcastQuote q state = do
  let bs = encode q
  -- T.putStrLn $ decodeUtf8 bs
  readMVar state >>= \clients ->
    forM_ clients $ \(_, conn) -> WS.sendTextData conn bs

handleDoc :: MVar ServerState -> Document -> IO ()
handleDoc state doc =
  let price = fromMaybe (doc !? "o.price") (doc !? "o2.price")
   in broadcastQuote (Quote (doc !? "o.instrumentSymbol") price) state

handleDocs :: MVar ServerState -> [Document] -> IO ()
handleDocs state = mapM_ (handleDoc state)

mongoThread :: MVar ServerState -> IO ()
mongoThread state = do
  (_threadId, _wait) <- forkOplogTail $ handleDocs state
  print "MongoThread started"
  return ()

-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

main :: IO ()
main = do
  print "DBG start of main"
  let (port :: Int) = 8080
  state <- newMVar newServerState
  mongoThread state
  print $ "WS server starting on port" <> show port
  let wsApp = application state
  let app = websocketsOr WS.defaultConnectionOptions wsApp myStaticApp
  run port app

-- WS.runServer "127.0.0.1" port $ application state

-- Our main application has the type:

application :: MVar ServerState -> WS.ServerApp
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
  conn <- WS.acceptRequest pending
  clients <- readMVar state
  let client = (pack ("User " <> show (length clients)), conn)
  WS.withPingThread conn 30 (return ()) $ do
    -- When a client is succesfully connected, we read the first message. This should
    -- be in the format of "Hi! I am Jasper", where Jasper is the requested username.
    print "DBG ping thread"
    flip finally (disconnect client) $ do
      -- We send a "Welcome!", according to our own little protocol. We add the client to
      -- the list and broadcast the fact that he has joined. Then, we give control to the
      -- 'talk' function.

      modifyMVar_ state $ \s -> do
        let s' = addClient client s
        WS.sendTextData conn $
          "Welcome! Users: "
            <> T.intercalate ", " (map fst s)
        broadcast (fst client <> " joined") s'
        return s'
      talk client state
  where
    disconnect client = do
      -- Remove client and return new state
      s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
      broadcast (fst client <> " disconnected") s

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state
    >>= broadcast
      (user `mappend` ": " `mappend` msg)

myStaticApp :: Application
myStaticApp = staticApp $ defaultFileServerSettings "./elm-client/build"
