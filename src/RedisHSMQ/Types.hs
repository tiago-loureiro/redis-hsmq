{-# LANGUAGE OverloadedStrings #-}

module RedisHSMQ.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Database.Redis.IO -- TODO: Types?

newtype QueueName = QueueName ByteString deriving (Eq, Show)
newtype QueueURL = QueueURL ByteString deriving (Eq, Show)

toKey :: QueueName -> Key
toKey (QueueName n) = Key (fromStrict n)
