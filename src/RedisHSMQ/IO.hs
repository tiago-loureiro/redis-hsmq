{-# LANGUAGE OverloadedStrings #-}

module RedisHSMQ.IO where

import Data.Monoid
import Database.Redis.IO
import Data.Int

import RedisHSMQ.Types

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
-- https://redis.io/commands/lpush
-- TODO: this should create a fresh random Message Id and write it into Message, to guarantee probabilistic uniqueness.
enqueue :: Monad m => QueueName -> Message -> Redis m Int64
enqueue n m = lpush (queuesDataPrefix n) (m :| [])

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
-- https://redis.io/commands/rpoplpush
pop :: Monad m => QueueName -> VisibilityTimeout -> Redis m (Maybe Message)
pop n _ = brpoplpush (queuesDataPrefix n) "worker" (Seconds 0)

pop' :: Monad m => QueueName -> Seconds -> VisibilityTimeout -> Redis m (Maybe Message)
pop' n s _ = brpoplpush (queuesDataPrefix n) "worker" s

-- https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
-- TODO: Check that this queue already exists
createQueue :: Monad m => QueueName -> m QueueURL
createQueue (QueueName n) = return $ QueueURL n

-- Get the messages currently being processed
inProcess :: Monad m => Redis m [Message]
inProcess = lrange "worker" 0 (-1)

queuesDataPrefix :: QueueName -> Key
queuesDataPrefix n = Key ("QUEUE:" <> qn)
  where
    Key qn = toKey n

queuesInfoPrefix :: QueueName -> Key
queuesInfoPrefix n = Key ("QUEUEINFO:" <> qn)
  where
    Key qn = toKey n

-- TODO: Maybe we should store this in a more structured way...
getQueueInfo :: Monad m => QueueName -> Redis m (Maybe QueueInfo)
getQueueInfo qn = get (queuesInfoPrefix qn)

addQueueInfo :: Monad m => QueueInfo -> Redis m Bool
addQueueInfo qi = set (queuesInfoPrefix $ qiName qi) qi nx
