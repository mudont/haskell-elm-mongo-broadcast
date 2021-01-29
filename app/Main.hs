module Main where

import ClassyPrelude
import Lib

getPort :: IO Int
getPort = return 8080

main :: IO ()
main = do
  port <- getPort

  defaultMain port
