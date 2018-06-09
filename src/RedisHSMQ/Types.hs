{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}

module RedisHSMQ.Types where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Database.Redis.IO -- TODO: Types?
import GHC.Generics (Generic)

import qualified Data.Text.Encoding as TE

newtype QueueName = QueueName ByteString deriving (Eq, Show)
newtype QueueURL = QueueURL ByteString deriving (Eq, Show)

toKey :: QueueName -> Key
toKey (QueueName n) = Key (fromStrict n)

data Message = Message
  { mBody :: Text
  , mId   :: Text
  } deriving (Eq, Generic, Show)

deriving instance FromJSON Message
deriving instance ToJSON Message

instance FromByteString Message where
    parser = parser >>= maybe (fail "Invalid message") return . parseMessage

parseMessage :: ByteString -> Maybe Message
parseMessage = decode . fromStrict

instance ToByteString Message where
    builder = builder . encode

