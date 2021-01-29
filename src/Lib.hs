{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Api
import App
import ClassyPrelude hiding (Handler, try)
import qualified ClassyPrelude as PL
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT (..))
import Crypto.JOSE (JWK)
import qualified Data.Map as M
import Data.Time.Clock
import Database.MongoDB
import Hubs (broadcastQuote)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy, simpleMethods)
import Oplog (Quote (..), forkOplogTail)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)

-- Natural transformation from AppM to Handler
toHandler :: State -> AppM a -> Handler a
toHandler state app = Handler $ ExceptT $ try $ runReaderT (runAppM app) state

corsMiddleware :: Middleware
corsMiddleware =
  cors
    ( const $
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = ["authorization", "content-type"],
              corsMethods = "PUT" : simpleMethods
            }
    )

mkApp :: State -> IO Application
mkApp state = do
  let cfg = EmptyContext
      api = Proxy :: Proxy Api
      context = Proxy :: Proxy '[]
  pure $ corsMiddleware $ serveWithContext api cfg $ hoistServerWithContext api context (toHandler state) server

runApp :: Int -> State -> IO ()
runApp port state =
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port $
            setBeforeMainLoop (print ("Listening on port " <> tshow port <> "...")) $ setLogger logger defaultSettings
    app <- mkApp state
    runSettings settings app

withState :: (State -> IO ()) -> IO ()
withState action = do
  userConnections <- newTVarIO M.empty
  action userConnections

handleDoc :: Document -> AppM ()
handleDoc doc =
  let price = fromMaybe (doc !? "o.price") (doc !? "o2.price")
   in broadcastQuote (Quote (doc !? "o.instrumentSymbol") price)

handleDocs :: [Document] -> AppM ()
handleDocs = mapM_ handleDoc

mongoBusiness :: State -> AppM ()
mongoBusiness state = do
  (_threadId, _wait) <- forkOplogTail $ \ds ->
    let (appRes :: ReaderT State IO ()) = runAppM (handleDocs ds)
     in do
          runReaderT appRes state
          return ()

  return ()

doMongoAndServantApp :: Int -> State -> IO ()
doMongoAndServantApp port state = do
  runReaderT (runAppM (mongoBusiness state)) state
  runApp port state

defaultMain :: Int -> IO ()
defaultMain port =
  withState (doMongoAndServantApp port)