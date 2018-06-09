{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}

module RedisHSMQ.Types where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Time.Clock
import Database.Redis.IO -- TODO: Types?
import GHC.Generics (Generic)
import Servant.API

import qualified Data.Text.Encoding as TE

newtype QueueName = QueueName ByteString deriving (Eq, Show)
newtype QueueURL = QueueURL ByteString deriving (Eq, Show)
newtype VisibilityTimeout = VisibilityTimeout Int deriving (Eq, Show, Generic)

instance FromHttpApiData QueueName where
    -- parseUrlPiece :: Text -> Either Text a
    parseUrlPiece = pure . QueueName . TE.encodeUtf8
    -- parseHeader :: ByteString -> Either Text a
    parseHeader = pure . QueueName
    -- parseQueryParam :: Text -> Either Text a
    parseQueryParam = parseUrlPiece

toKey :: QueueName -> Key
toKey (QueueName n) = Key (fromStrict n)

data Message = Message
  { mBody    :: Text
  , mId      :: Text
  } deriving (Eq, Generic, Show)

deriving instance FromJSON Message
deriving instance ToJSON Message

instance FromByteString Message where
    parser = parser >>= maybe (fail "Invalid message") return . parseMessage

parseMessage :: ByteString -> Maybe Message
parseMessage = decode . fromStrict

instance ToByteString Message where
    builder = builder . encode

data InProcessMessage = InProcessMessage
  { ipMessage :: Message
  , ipExpires :: UTCTime
  } deriving (Eq, Generic, Show)
