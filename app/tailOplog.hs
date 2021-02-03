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

module Main where

-- import ClassyPrelude hiding (bracket, find)

import Config
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Bson
import Data.Time.Clock
import Database.MongoDB
import Oplog

main :: IO ()
main = do
  print $ "main" ++ "Starting..."
  (threadId, wait) <- Thread.forkIO $ bracket (connect mongoHost) close $ \pipe -> tailOpLog pipe print
  print $ "Thread forked " ++ show threadId
  result <- wait
  print $ "Thread done. Result = " ++ show result
