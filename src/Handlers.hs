module Handlers where

import App (State)
import ClassyPrelude
import Control.Monad.Catch (MonadThrow)
import qualified Data.UUID.V4 as UUID
import Hubs (HubConnectionId, HubType (..))
import qualified Hubs as H
import Network.WebSockets (PendingConnection, acceptRequest)

newtype HandlerM a = HandlerM
  { runHandlerM :: ReaderT State IO a
  }
  deriving (Functor, Applicative, Monad, MonadReader State, MonadThrow, MonadIO, MonadUnliftIO)

subscribeToQuotes :: HubType -> PendingConnection -> HandlerM HubConnectionId
subscribeToQuotes hub pConn = do
  conn <- liftIO $ acceptRequest pConn
  connId <- liftIO UUID.nextRandom
  H.subscribeToHub hub conn connId
  H.keepConnectionAlive conn connId
  return connId

liftHandler :: (MonadIO m, MonadReader State m, MonadThrow m) => HandlerM a -> m a
liftHandler operation = do
  state <- ask
  liftIO $ flip runReaderT state $ runHandlerM operation