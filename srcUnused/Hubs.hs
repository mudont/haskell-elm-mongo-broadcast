{-# LANGUAGE ScopedTypeVariables #-}

module Hubs where

import ClassyPrelude
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), encode)
import Data.Has
import qualified Data.Map as M
import qualified Data.Maybe as May (fromJust)
import Data.UUID (UUID, toText)
import Json
import Network.WebSockets
import Oplog (Quote)

data HubType
  = QuotesHub
  deriving (Eq, Ord, Show)

data NotificationType
  = InvitedToGame
  | GameStarted
  | InvitationDeclined
  deriving (Generic, Show)

instance ToJSON NotificationType where
  toJSON val = String (tshow val)

data Notification = Notification
  { notificationId :: UUID,
    notificationType :: NotificationType,
    notificationPayload :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON Notification where
  toJSON = genericToJSONNoPrefix "notification"

type HubsState = TVar Hubs

type Hubs = M.Map HubType [HubConnection]

type HubConnectionId = UUID

data HubConnection = HubConnection
  { hubConnId :: UUID,
    hubConnConn :: Connection
  }

instance Show HubConnection where
  show (HubConnection id _) = "Connection Id: " <> show id

type HubOperation r m = (Has HubsState r, MonadReader r m, MonadIO m)

subscribeToHub :: HubOperation r m => HubType -> Connection -> HubConnectionId -> m ()
subscribeToHub hub conn connId =
  withHubsDo $ \(hubs, hState) ->
    atomically $ modifyTVar hState (M.insertWith (<>) hub [HubConnection connId conn])

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- NOTE: <Haskell Rookie hack MRD 2021/01/30>
-- Struggled to replace the call to deprecated forkPingThread in keepConnectionAlive
-- with call to withPingThread
-- The former was starightforward IO monad operation that could be liftIO'd to m ()
-- The latter takes the rest the function as a function argument and expects that funtion
-- to return IO (), but we need to work in our App monad so we can use State to get at the hubState
-- Don't know how to deal with that. Some guessed didn't work. So I get the state out beforehand
-- and pass it explicitly to new functions that run in IO ()
--
-- Hopefully I will come back here someday and laugh at how naive this was
--
-- keepConnectionAlive :: (MonadUnliftIO m, HubOperation r m) => Connection -> HubConnectionId -> m ()
-- keepConnectionAlive conn connId = do
--   liftIO $ forkPingThread conn 30
--   keepListening conn `catch` handleClosed connId
--
-- removeConnection :: HubOperation r m => HubConnectionId -> m ()
-- removeConnection connectionId =
--   withHubsDo $ \(hubs, hState) ->
--     atomically $ modifyTVar hState (M.map (filter (\(HubConnection id _) -> id /= connectionId)))

-- handleClosed :: HubOperation r m => HubConnectionId -> ConnectionException -> m ()
-- handleClosed c (CloseRequest _ _) = removeConnection c
-- handleClosed c ConnectionClosed = removeConnection c

-- keepListening :: HubOperation r m => Connection -> m ()
-- keepListening conn = liftIO $ forever $ void (receiveData conn :: IO Text)

keepListeningNoFancyMonad :: Connection -> IO ()
keepListeningNoFancyMonad conn = liftIO $ forever $ void (receiveData conn :: IO Text)

removeConnectionNoFancyMonad :: HubsState -> HubConnectionId -> ConnectionException -> IO ()
removeConnectionNoFancyMonad hState connectionId (CloseRequest _ _) =
  atomically $ modifyTVar hState (M.map (filter (\(HubConnection id _) -> id /= connectionId)))
removeConnectionNoFancyMonad hState connectionId ConnectionClosed =
  atomically $ modifyTVar hState (M.map (filter (\(HubConnection id _) -> id /= connectionId)))

keepConnectionAlive :: (MonadUnliftIO m, HubOperation r m) => Connection -> HubConnectionId -> m ()
keepConnectionAlive conn connId = do
  -- Get damn hState out so we can run the rest of the function in plain IO monad
  -- Don't know how else to satisfy withPingThread
  hState <- withHubsDo $ \(_, hState) -> return hState
  --hubsMap <- liftIO . readTVarIO $ hState
  liftIO $ withPingThread conn 30 (pure ()) (keepListeningNoFancyMonad conn) `catch` removeConnectionNoFancyMonad hState connId

--  END: </Haskell Rookie hack MRD>
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

broadcastQuote :: HubOperation r m => Quote -> m ()
broadcastQuote message =
  withHubsDo $ \(hubs, _) -> do
    let hubConnections = map hubConnConn <$> M.lookup QuotesHub hubs
    forM_ hubConnections (mapM_ (liftIO . (`sendTextData` json)))
  where
    json = encode message

withHubsDo :: HubOperation r m => ((Hubs, HubsState) -> m a) -> m a
withHubsDo action = do
  hubsState <- asks getter
  hubsMap <- liftIO . readTVarIO $ hubsState
  action (hubsMap, hubsState)