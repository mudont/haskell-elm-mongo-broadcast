{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoConstraintKinds #-}
-- {-# LANGUAGE NoDataKinds #-}
-- {-# LANGUAGE NoDeriveGeneric #-}
-- {-# LANGUAGE NoFlexibleContexts #-}
-- {-# LANGUAGE NoFlexibleInstances #-}
-- {-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE NoMultiParamTypeClasses #-}
-- {-# LANGUAGE NoTemplateHaskell #-}
-- {-# LANGUAGE NoTypeFamilies #-}
-- {-# LANGUAGE NoTypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- stack script --resolver lts-16.11

module Oplog where

-- import ClassyPrelude hiding (bracket, find)
import ClassyPrelude
import qualified ClassyPrelude as PL
import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
-- import Control.Exception ( bracket )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Text ()
import Database.MongoDB (Field ((:=)), (=:))
import qualified Database.MongoDB as Mongo
import Json

-- import GHC.Types

localDb :: Mongo.Database
localDb = "local"

opLogColl :: Mongo.Collection
opLogColl = "oplog.rs"

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
      Mongo.access pipe Mongo.master localDb . Mongo.find $
        ( Mongo.select
            [ "ns" =: "quotes.quote",
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

-- run :: (MonadUnliftIO m, MonadIO m) => (Mongo.Action m Mongo.Cursor -> m Mongo.Cursor) = Mongo.access pipe Mongo.master localDb
-- run2 :: (MonadUnliftIO m, MonadIO m) => (Mongo.Action m () -> m ()) = Mongo.access pipe Mongo.master localDb

forkOplogTail :: (MonadUnliftIO m, MonadIO m) => ([Mongo.Document] -> IO ()) -> m (ThreadId, IO (Thread.Result ()))
forkOplogTail f =
  liftIO $
    Thread.forkIO $
      bracket (Mongo.connect (Mongo.host "127.0.0.1")) Mongo.close $
        \pipe -> do
          tailOpLog pipe f
          return ()

-- main :: IO ()
-- main = do
--   print $ "main" ++ "Starting..."
--   (threadId, wait) <- Thread.forkIO $ bracket (Mongo.connect (Mongo.host "127.0.0.1")) Mongo.close $ \pipe -> tailOpLog pipe print
--   print $ "Thread forked " ++ show threadId
--   result <- wait
--   print $ "Thread done. Result = " ++ show result
