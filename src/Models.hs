{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models where

-- import ClassyPrelude hiding (bracket, find)
import ClassyPrelude
-- import Control.Exception ( bracket )

import qualified ClassyPrelude as Data.ByteString.Lazy.Internal
import qualified ClassyPrelude as PL
import Config
import Control.Concurrent
import qualified Control.Concurrent.Thread as Thread
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.AesonBson (aesonify, aesonifyValue)
import Data.Text ()
import Database.MongoDB (Document, Field ((:=)), Val (val), (!?), (=:))
import qualified Database.MongoDB as Mongo
import Json

-- import GHC.Types

data Quote = Quote
  { instrumentSymbol :: Maybe Text,
    price :: Maybe Double,
    collType :: Text
  }
  deriving (Generic, Show)

instance ToJSON Quote where
  toJSON = genericToJSONNoPrefix ""

instance FromJSON Quote

docToQuote :: Document -> Quote
docToQuote doc =
  let pr = fromMaybe (doc !? "o.price") (doc !? "o2.price")
   in Quote (doc !? "o.instrumentSymbol") pr quoteColl

-- | docToQuote' doesnt work
-- For some reason, code seems to hang when accessing this data
docToQuote' :: Document -> Result Quote
docToQuote' d =
  fromJSON $ Object $ aesonify d

data Security = Security
  { securitySymbol :: Maybe Text,
    askSizes :: [Int],
    askPrices :: [Double],
    bidSizes :: [Int],
    bidPrices :: [Double],
    collType :: Text
  }
  deriving (Generic, Show)

instance ToJSON Security where
  toJSON = genericToJSONNoPrefix ""

instance FromJSON Security

docToSecurity :: Document -> Security
docToSecurity doc =
  let aS = fromMaybe [] $ fromMaybe (doc !? "o.ask_sizes") (doc !? "o2.ask_sizes")
      bS = fromMaybe [] $ fromMaybe (doc !? "o.bid_sizes") (doc !? "o2.bid_sizes")
      aP = fromMaybe [] $ fromMaybe (doc !? "o.ask_prices") (doc !? "o2.ask_prices")
      bP = fromMaybe [] $ fromMaybe (doc !? "o.bid_prices") (doc !? "o2.bid_prices")
   in Security (doc !? "o.sym") aS aP bS bP securityColl
