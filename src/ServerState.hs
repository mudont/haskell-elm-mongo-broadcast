{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ServerState where

import ClassyPrelude
import Control.Lens hiding (element)
import qualified Network.WebSockets as WS

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.

type Client = (Text, WS.Connection)

data ServerState = ServerState {_nextId :: Int, _clients :: [Client]}

$(makeLenses ''ServerState)

-- Create a new, initial state:

newServerState :: ServerState
newServerState = ServerState 1 []

-- Get the number of active clients:

numClients :: ServerState -> Int
numClients st = length $ st ^. clients

-- Check if a user already exists (based on username):

clientExists :: Client -> ServerState -> Bool
clientExists client state = any ((== fst client) . fst) (state ^. clients)

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: Client -> ServerState -> ServerState
addClient client = over clients (client :)

bumpNextId :: ServerState -> ServerState
bumpNextId = over nextId (+ 1)

-- Remove a client:

removeClient :: Client -> ServerState -> ServerState
removeClient client = over clients $ filter ((/= fst client) . fst)
