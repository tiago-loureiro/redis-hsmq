{-# LANGUAGE OverloadedStrings #-}

module RedisHSMQ.IO where

import Database.Redis.IO
import Data.Int

import RedisHSMQ.Types

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
-- https://redis.io/commands/lpush
enqueue :: Monad m => QueueName -> Message -> Redis m Int64
enqueue n m = lpush (toKey n) (m :| [])

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
-- https://redis.io/commands/rpoplpush
pop :: Monad m => QueueName -> VisibilityTimeout -> Redis m (Maybe Message)
pop n _ = brpoplpush (toKey n) "worker" (Seconds 0)

pop' :: Monad m => QueueName -> Seconds -> VisibilityTimeout -> Redis m (Maybe Message)
pop' n s _ = brpoplpush (toKey n) "worker" s

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
-- TODO: Check that this queue already exists
createQueue :: Monad m => QueueName -> m QueueURL
createQueue (QueueName n) = return $ QueueURL n

-- Get the messages currently being processed
inProcess :: Monad m => Redis m [Message]
inProcess = lrange "worker" 0 (-1)
