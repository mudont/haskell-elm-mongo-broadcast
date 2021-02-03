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
{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude hiding (forM_)
import Config
import Control.Lens hiding (element)
import Control.Lens.TH
import Control.Monad (forM_, forever)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
import Data.AesonBson
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.MongoDB
import Models
import Network.Wai (Application)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Oplog
import ServerState
import Prelude (read, (!!))

-- Send a message to all clients, and log it on stdout:

broadcast :: (WS.WebSocketsData a, Show a) => a -> ServerState -> IO ()
broadcast message st = forM_ (st ^. clients) $ \(_, conn) -> WS.sendTextData conn message

-- broadcastQuote :: Quote -> MVar ServerState -> IO ()
-- broadcastQuote q state = do
--   let bs = encode q
--   readMVar state >>= \clients ->
--     forM_ clients $ \(_, conn) -> WS.sendTextData conn bs

-- docToBS :: Document -> ByteString
-- docToBS doc =
--     let ns = fromMaybe "" (doc !? "ns")
--       in case ns of
--           quoteColl -> encode $ docToQuote doc
--           securityColl -> encode $ docToQuote doc
--           _ -> ""

broadcastDoc :: ServerState -> Document -> IO ()
broadcastDoc st doc =
  let ns :: Text = fromMaybe "" (doc !? "ns")
   in case ns of
        _
          | ns == quoteNs -> broadcast (encode $ docToQuote doc) st
          | ns == securityNs -> broadcast (encode $ docToSecurity doc) st
          | otherwise -> do
            print $ ns <> ": unsupported collection"
            return ()

-- let resQuote = docToQuote doc
-- print "DBG"
-- case resQuote of
--   Error e -> print e
--   Success q -> broadcast (encode q) st

-- let val = aesonify doc
-- print $ show val
-- broadcast (Data.Text.Conversions.toText $ show val) st

-- broadcast bs st
-- let conns = map snd $ st ^. clients
-- print $ show (length conns) <> " clients"
-- forM_ [1 .. length conns] $ \n -> do
--   print $ "dummy loop " <> show (n -1) <> show bs
--   print n
--   WS.sendTextData (conns !! (n -1)) bs
-- forM_ conns $ \conn -> do
--   print $ "Sending " <> bs
--   WS.sendTextData conn bs
-- print "Done broadcast"

handleDocs :: ServerState -> [Document] -> IO ()
handleDocs state = mapM_ (broadcastDoc state)

mongoThread :: MVar ServerState -> IO ()
mongoThread state = do
  (_threadId, _wait) <- forkOplogTail $ \docs -> readMVar state >>= \s -> handleDocs s docs
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
  print $ "WS server starting on port " <> show port
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

application stateM pending = do
  conn <- WS.acceptRequest pending
  state <- readMVar stateM
  -- Make up a name for client - "User <UniqueNum>"
  let client = (pack ("User " <> show (state ^. nextId)), conn)
  WS.withPingThread conn 30 (return ()) $ do
    -- When a client is succesfully connected, we read the first message. This should
    -- be in the format of "Hi! I am Jasper", where Jasper is the requested username.
    print "DBG ping thread"
    flip finally (disconnect client) $ do
      modifyMVar_ stateM $ \s -> do
        let s' = addClient client s
        WS.sendTextData conn $
          "Welcome! Users: "
            <> T.intercalate ", " (map fst (s ^. clients))
        broadcast (fst client <> " joined") s
        return (bumpNextId s')
      talk client stateM
  where
    disconnect client = do
      -- Remove client and return new state
      s'' <- modifyMVar stateM $ \s -> let s' = removeClient client s in return (s', s')
      broadcast (fst client <> " disconnected") s''

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) stateM = forever $ do
  msg <- WS.receiveData conn
  readMVar stateM
    >>= \s ->
      broadcast
        (user `mappend` ": " `mappend` msg)
        s

myStaticApp :: Application
myStaticApp = staticApp $ defaultFileServerSettings "./elm-client/build"
