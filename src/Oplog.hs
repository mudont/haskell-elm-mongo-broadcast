{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- stack script --resolver lts-16.11

module Oplog where

-- import ClassyPrelude hiding (bracket, find)
import ClassyPrelude
import qualified ClassyPrelude as PL
-- import Control.Exception ( bracket )

import Config
import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Text ()
import Database.MongoDB (Field ((:=)), (=:))
import qualified Database.MongoDB as Mongo
import Json

-- import GHC.Types

data Quote = Quote
  { instrumentSymbol :: Maybe Text,
    price :: Maybe Double
  }
  deriving (Generic, Show)

instance ToJSON Quote where
  toJSON = genericToJSONNoPrefix ""

tailOpLog :: (MonadUnliftIO m, MonadIO m) => Mongo.Pipe -> ([Mongo.Document] -> IO ()) -> m ()
tailOpLog pipe f = do
  bracket acquire release (Mongo.access pipe Mongo.master localDb . loop)
  return ()
  where
    acquire =
      -- We pick up inserts and updates to the single collection quotes.quote
      Mongo.access pipe Mongo.master localDb . Mongo.find $
        ( Mongo.select
            [ "ns" =: quoteNs,
              "$or" =: [["op" =: "i"], ["op" =: "u"]]
            ]
            opLogColl
        )
          { Mongo.options = [Mongo.TailableCursor, Mongo.AwaitData, Mongo.NoCursorTimeout]
          }
    release x = Mongo.access pipe Mongo.master localDb $ Mongo.closeCursor x
    loop cr = do
      xs <- Mongo.nextBatch cr
      if PL.null xs
        then return ()
        else liftIO $ f xs
      loop cr

forkOplogTail :: (MonadUnliftIO m, MonadIO m) => ([Mongo.Document] -> IO ()) -> m (ThreadId, IO (Thread.Result ()))
forkOplogTail f =
  liftIO $
    Thread.forkIO $
      bracket (Mongo.connect mongoHost) Mongo.close $
        \pipe -> do
          tailOpLog pipe f
          return ()
