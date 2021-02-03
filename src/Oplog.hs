{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Models

-- import GHC.Types

{-
{ "spn" : 65939755, "ADV" : 0, "ADVMUL" : 1,
"ask" : 53.29, "ask_prices" : [ 53.29, 53.3, 53.31, 53.32, 53.33, 53.34, 53.35, 53.36, 53.37, 53.38 ],
"ask_size" : [ ],
"atm_implied_vol" : -1, "bid" : 53.27,
"bid_prices" : [ 53.27, 53.26, 53.25, 53.24, 53.23, 53.22, 53.21, 53.2, 53.19, 53.18 ],
"bid_size" : [ ],
"clearing_method" : "NYMEX", "close_price" : 52.9, "commodity_type" : "Other", "death" : "20210520", "energy_region" : "Gulf/Texas", "exchange_identifier" : "CL", "exchange_product_id" : null, "gamma" : 0, "implied_vol" : 0, "instr_ref_price" : 0, "instr_tick_sz" : 0.01, "is_atomic" : true, "location" : null, "lot_increment_size" : 1, "lot_size" : 1, "mkt_implied_vol" : -1, "month" : null, "peak_type" : null, "put_call_ind" : null, "qty_min_increment" : 0.000001, "quote_exchange" : "CME_ENERGY", "quote_factor" : 1, "sectype" : "F", "size_factor" : 1000, "strike" : null, "subtype" : "Commodity Future", "sym" : "CLM1", "tickunit" : 1000, "timestamp" : "20210201@204040.422856@EST", "type" : "Fut", "underlying_ref_price" : 0, "adv" : 0,
"ask_sizes" : [ 10, 48, 1, 3, 2, 4, 3, 3, 2, 2 ],
"bid_sizes" : [ 15, 24, 1, 3, 3, 1, 4, 3, 3, 4 ],
"cum_vol" : 1949, "date" : "20210201", "instr_expiry" : "20210520", "instr_type" : 3, "instr_type_str" : "FUTURE", "last_price" : 53.29, "last_size" : 1 }

-}

tailOpLog :: (MonadUnliftIO m, MonadIO m) => Mongo.Pipe -> ([Mongo.Document] -> IO ()) -> m ()
tailOpLog pipe f = do
  bracket acquire release (Mongo.access pipe Mongo.master localDb . loop)
  return ()
  where
    acquire = do
      currTime <- liftIO getCurrentTime
      liftIO $ print currTime
      -- We pick up inserts and updates to the single collection quotes.quote
      Mongo.access pipe Mongo.master localDb . Mongo.find $
        ( Mongo.select
            [ "$or" =: [["ns" =: quoteNs], ["ns" =: securityNs]],
              -- "ns" =: securityNs,
              "$or" =: [["op" =: "i"], ["op" =: "u"]],
              "wall" =: ["$gt" =: currTime]
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
