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

subscribeToHub :: HubOperation r m => HubType -> Connection -> HubConnectionId -> m () -- <-- TODO: Either?
subscribeToHub hub conn connId =
  withHubsDo $ \(hubs, hState) ->
    atomically $ modifyTVar hState (M.insertWith (<>) hub [HubConnection connId conn])

removeConnection :: HubOperation r m => HubConnectionId -> m ()
removeConnection connectionId =
  withHubsDo $ \(hubs, hState) ->
    atomically $ modifyTVar hState (M.map (filter (\(HubConnection id _) -> id /= connectionId)))

keepConnectionAlive :: (MonadUnliftIO m, HubOperation r m) => Connection -> HubConnectionId -> m ()
keepConnectionAlive conn connId = do
  liftIO $ forkPingThread conn 30
  keepListening `catch` handleClosed
  where
    keepListening = liftIO $ forever $ void (receiveData conn :: IO Text)
    handleClosed (CloseRequest _ _) = removeConnection connId
    handleClosed ConnectionClosed = removeConnection connId

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