module Config where

import ClassyPrelude
import Database.MongoDB

mongoPort :: PortID
mongoPort = PortNumber 27017

mongoHost :: Host
mongoHost = Host "127.0.0.1" mongoPort

numUpdateCycles :: Int
numUpdateCycles = 25

pauseBetweenUpdatesMicrosecs :: Int
pauseBetweenUpdatesMicrosecs = 1000 * 1000 -- 1000*1000 = 1 second

localDb :: Database
localDb = "local"

opLogColl :: Collection
opLogColl = "oplog.rs"

db :: Database
db = "quotes"

collName :: Text
collName = "quote"

quoteNs :: Text
quoteNs = db <> "." <> collName

initialRows :: [(String, Float)]
initialRows =
  [ ("IBM", 63.12),
    ("MSFT", 73.12),
    ("T", 83.12),
    ("AMZN", 13.12),
    ("WMT", 23.12),
    ("CLZ4", 63.12),
    ("GOOG", 663.12)
  ]