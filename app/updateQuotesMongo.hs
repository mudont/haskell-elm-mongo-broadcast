#!/usr/bin/env stack
-- stack script --resolver lts-17

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import ClassyPrelude hiding (delete, find, sort)
import Config
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Data.Functor
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

main :: IO ()
main = do
  pipe <- connect mongoHost
  e <- access pipe master db $ run numUpdateCycles
  close pipe
  print e

run :: Int -> Action IO ()
run max = do
  let loop n = do
        --clearQuotes
        -- insertQuotes
        insertSecs
        --allQuotes >>= printDocs "All Quotes"
        liftIO $ threadDelay pauseBetweenUpdatesMicrosecs
        -- print n
        when (n `mod` 1000 == 0) $ print n
        when (n < max) $ loop (n + 1)
  loop 1

clearQuotes :: Action IO ()
clearQuotes = delete (select [] quoteColl)

getNRands :: Int -> IO [Float]
getNRands = ($ randomRIO (-1.0, 1 :: Float)) . replicateM

getNRandInts :: Int -> IO [Int]
getNRandInts = ($ randomRIO (1, 25 :: Int)) . replicateM

getRows :: IO [(Selector, Document, [UpdateOption])]
getRows = do
  rands <- getNRands $ length initialRows
  return $
    map (\r -> (["instrumentSymbol" =: fst r], ["instrumentSymbol" =: fst r, "price" =: snd r], [Upsert])) $
      zipWith (\r p -> (fst r, snd r + p)) initialRows rands

insertQuotes :: Action IO WriteResult
insertQuotes = do
  rows <- liftIO getRows
  updateMany quoteColl rows

getSecRows :: IO [(Selector, Document, [UpdateOption])]
getSecRows = do
  rands <- getNRands $ length initialSecRows
  randInts <- getNRandInts $ length initialSecRows
  return $
    map
      ( \(sym, bS, bP, aS, aP) ->
          ( ["sym" =: sym],
            [ "sym" =: sym,
              "bid_sizes" =: bS,
              "bid_prices" =: bP,
              "ask_sizes" =: aS,
              "ask_prices" =: aP
            ],
            [Upsert]
          )
      )
      $ zipWith
        ( \(sym, bS, bP, aS, aP) (p, i) ->
            ( sym,
              map (+ i) bS,
              map (+ p) bP,
              map (+ i) aS,
              map (+ p) aP
            )
        )
        initialSecRows
        $ zip rands randInts

insertSecs :: Action IO WriteResult
insertSecs = do
  rows <- liftIO getSecRows
  updateMany securityColl rows

allQuotes :: Action IO [Document]
allQuotes = rest =<< find (select [] quoteColl) {sort = ["instrumentSymbol" =: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ print title >> mapM_ (print . exclude ["_id"]) docs