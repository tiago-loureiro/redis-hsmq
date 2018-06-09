{-# LANGUAGE OverloadedStrings #-}

module RedisHSMQ.IO where

import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Database.Redis.IO
import Data.Int
import Data.List.NonEmpty

import RedisHSMQ.Types

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
-- https://redis.io/commands/lpush
enqueue :: Monad m => QueueName -> Message -> Redis m Int64
enqueue n m = lpush (toKey n) (m :| [])

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
-- https://redis.io/commands/rpoplpush
pop :: Monad m => QueueName -> Redis m (Maybe Message)
pop n = brpoplpush (toKey n) "worker" (Seconds 0)

pop' :: Monad m => QueueName -> Seconds -> Redis m (Maybe Message)
pop' n s = brpoplpush (toKey n) "worker" s

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
-- TODO: Check that this queue already exists
createQueue :: Monad m => QueueName -> m QueueURL
createQueue (QueueName n) = return $ QueueURL n
