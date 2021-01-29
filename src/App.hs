module App where

import ClassyPrelude
import Control.Monad.Catch (MonadThrow)
import Hubs
import Servant.Auth.Server (JWTSettings)

type State = (HubsState)

newtype AppM a = AppM
  { runAppM :: ReaderT State IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader State, MonadThrow, MonadUnliftIO)