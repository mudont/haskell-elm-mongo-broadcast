{-# LANGUAGE ScopedTypeVariables #-}

module Api
  ( Api,
    server,
  )
where

import App
import ClassyPrelude
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson (ToJSON, encode)
import Data.CaseInsensitive (mk)
import Error (AppError (..))
import Handlers (HandlerM)
import qualified Handlers
import qualified Hubs as H
import Network.WebSockets (PendingConnection)
import Servant hiding (Unauthorized)
import Servant.API.WebSocket
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type Api = "quotes" :> "subscribe" :> WebSocketPending :<|> Raw

type AppServer api = ServerT api AppM

server :: AppServer Api
server =
  subscribeToQuotes :<|> serveDirectoryWith ((defaultFileServerSettings "./elm-client/build") {ssRedirectToIndex = False})

subscribeToQuotes :: (MonadReader State f, MonadThrow f, MonadUnliftIO f) => PendingConnection -> f ()
subscribeToQuotes pConn = void $ runHandlerOp $ Handlers.subscribeToQuotes H.QuotesHub pConn

newtype ApiError = ApiError
  { message :: Text
  }
  deriving (Generic)

instance ToJSON ApiError

runHandlerOp :: (MonadIO m, MonadReader State m, MonadThrow m, MonadUnliftIO m) => HandlerM a -> m a
runHandlerOp handlerOp =
  Handlers.liftHandler handlerOp `catch` (\(ex :: AppError) -> handleError ex)
  where
    handleError ex = throwM $ toServantError ex

toServantError :: AppError -> ServerError
toServantError (ValidationError msg) = servantErrorWithText err400 msg
toServantError (Unauthorized msg) = servantErrorWithText err401 msg
toServantError (NotFound msg) = servantErrorWithText err404 msg
toServantError (Conflict msg) = servantErrorWithText err409 msg

servantErrorWithText :: ServerError -> Text -> ServerError
servantErrorWithText sErr message = sErr {errBody = errorBody, errHeaders = [jsonHeaders]}
  where
    errorBody = encode $ ApiError message
    jsonHeaders = (mk "Content-Type", "application/json;charset=utf-8")