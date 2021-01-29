#!/usr/bin/env stack
-- stack script --resolver lts-17.0

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoConstraintKinds #-}
{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE NoDeriveGeneric #-}
{-# LANGUAGE NoFlexibleContexts #-}
{-# LANGUAGE NoFlexibleInstances #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMultiParamTypeClasses #-}
{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE NoTypeFamilies #-}
{-# LANGUAGE NoTypeOperators #-}
{-# LANGUAGE ImplicitPrelude #-}

-- stack script --resolver lts-16.11

module Main where

-- import ClassyPrelude hiding (bracket, find)

import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bson as Bson
import Database.MongoDB (Action, Collection, Cursor, Database, Document, Field ((:=)), Pipe, QueryOption (AwaitData, NoCursorTimeout, TailableCursor), access, close, closeCursor, connect, find, host, master, nextBatch, options, select, (=:))
import qualified Database.MongoDB as Mongo

-- import GHC.Types

localDb :: Database
localDb = "local"

opLogColl :: Collection
opLogColl = "oplog.rs"

tailOpLog :: Pipe -> ([Document] -> IO ()) -> IO ()
tailOpLog pipe f = do
  bracket acquire release (run . loop)
  putStrLn "Done"
  where
    foo = (select ["ns" =: "murali.quote", "op" =: "i"] opLogColl) {options = [TailableCursor, AwaitData, NoCursorTimeout]}
    foo2 = find foo
    acquire = run foo2
    release x = do
      print $ "mongodb: " ++ "Closing opLog cursor..."
      run2 $ closeCursor x
    loop cr = do
      xs <- nextBatch cr
      liftIO $
        if null xs
          then return ()
          else do
            print $ "mongodb: " ++ "nextBatch.length=" ++ show (length xs)
            f xs
      loop cr
    run :: (Action IO Cursor -> IO Cursor) = access pipe master localDb
    run2 :: (Action IO () -> IO ()) = access pipe master localDb

main :: IO ()
main = do
  print $ "main" ++ "Starting..."
  (threadId, wait) <- Thread.forkIO $ bracket (connect (host "127.0.0.1")) close $ \pipe -> tailOpLog pipe print
  print $ "Thread forked " ++ show threadId
  result <- wait
  print $ "Thread done. Result = " ++ show result
