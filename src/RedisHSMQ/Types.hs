{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}

module RedisHSMQ.Types where

-- TODO: Since we are storing JSON as bytestring, we need to make
--       sure that certain characters are not allowed, for instance,
--       in queue names

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.UUID
import Data.Text (Text)
import Data.Time.Clock
import Database.Redis.IO -- TODO: Types?
import GHC.Generics (Generic)
import Servant.API

import qualified Data.Text.Encoding as TE

newtype QueueName = QueueName Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype QueueURL = QueueURL Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
newtype VisibilityTimeout = VisibilityTimeout NominalDiffTime deriving (Eq, Show, Generic)
newtype EndOfLife = EndOfLife UTCTime deriving (Eq, Show, Generic)

instance FromHttpApiData QueueName where
    -- parseUrlPiece :: Text -> Either Text a
    parseUrlPiece = pure . QueueName
    -- parseHeader :: ByteString -> Either Text a
    parseHeader = pure . QueueName . TE.decodeUtf8
    -- parseQueryParam :: Text -> Either Text a
    parseQueryParam = parseUrlPiece

toKey :: QueueName -> Key
toKey (QueueName n) = Key (fromStrict $ TE.encodeUtf8 n)

toField :: QueueName -> Field
toField (QueueName n) = fromStrict $ TE.encodeUtf8 n

data QueueInfo = QueueInfo
  { qiName       :: QueueName
  , qiDefTimeout :: VisibilityTimeout
  } deriving (Eq, Generic, Show)

deriving instance FromJSON QueueInfo
deriving instance ToJSON QueueInfo

parseQueueInfo :: ByteString -> Maybe QueueInfo
parseQueueInfo = decode . fromStrict

instance FromByteString QueueInfo where
    parser = parser >>= maybe (fail "Invalid QueueInfo") return . decode . fromStrict

instance ToByteString QueueInfo where
    builder = builder . encode

data Message = Message
  { mBody    :: Text
  , mId      :: UUID
  , mTimeout :: VisibilityTimeout
  } deriving (Eq, Ord, Generic, Show)

deriving instance FromJSON Message
deriving instance ToJSON Message

deriving instance FromJSON VisibilityTimeout
deriving instance ToJSON VisibilityTimeout

deriving instance FromJSON EndOfLife
deriving instance ToJSON EndOfLife

instance FromByteString Message where
    parser = parser >>= maybe (fail "Invalid message") return . parseMessage

parseMessage :: ByteString -> Maybe Message
parseMessage = decode . fromStrict

instance ToByteString Message where
    builder = builder . encode

instance FromByteString EndOfLife where
    parser = parser >>= maybe (fail "Invalid EndOfLife") return . decode . fromStrict

instance ToByteString EndOfLife where
    builder = builder . encode
