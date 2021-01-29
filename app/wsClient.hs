#!/usr/bin/env stack
-- stack script --resolver lts-17.0

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
  T.putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $
    forever $ do
      msg <- WS.receiveData conn
      liftIO $ T.putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line
        loop
  T.putStrLn "Loop"
  loop
  WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/quotes/subscribe?access-token=MRD" app
