#!/usr/bin/env stack
-- stack script --resolver lts-16.27

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import ClassyPrelude hiding (delete, find, sort)
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Database.MongoDB
  ( Action,
    Document,
    Value,
    access,
    close,
    connect,
    delete,
    exclude,
    find,
    host,
    insertMany,
    master,
    project,
    rest,
    select,
    sort,
    (=:),
  )
import Database.MongoDB.Query
import System.Random

db :: Text
db = "quotes"

collName :: Text
collName = "quote"

main :: IO ()
main = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master db $ run (25 * 1)
  close pipe
  print e

run :: Int -> Action IO ()
run max = do
  let loop n = do
        --clearQuotes
        insertQuotes
        --allQuotes >>= printDocs "All Quotes"
        liftIO $ threadDelay (1000 * 1000) -- micro secs
        -- print n
        when (n `mod` 1000 == 0) $ print n
        when (n < max) $ loop (n + 1)
  loop 1

clearQuotes :: Action IO ()
clearQuotes = delete (select [] collName)

getNRands :: Int -> IO [Float]
getNRands = ($ randomRIO (-1.0, 1 :: Float)) . replicateM

getRows :: IO [(Selector, Document, [UpdateOption])]
getRows = do
  rands <- getNRands $ length rows
  return $
    map (\r -> (["instrumentSymbol" =: fst r], ["instrumentSymbol" =: fst r, "price" =: snd r], [Upsert])) $
      zipWith (\r p -> (fst r, snd r + p)) rows rands
  where
    rows :: [(String, Float)] =
      [ ("IBM", 63.12),
        ("MSFT", 73.12),
        ("T", 83.12),
        ("AMZN", 13.12),
        ("WMT", 23.12),
        ("CLZ4", 63.12),
        ("GOOG", 663.12)
      ]

insertQuotes :: Action IO WriteResult
insertQuotes = do
  rows <- liftIO getRows
  updateMany collName rows

allQuotes :: Action IO [Document]
allQuotes = rest =<< find (select [] collName) {sort = ["instrumentSymbol" =: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ print title >> mapM_ (print . exclude ["_id"]) docs