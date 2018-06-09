{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Database.Redis.IO
import RedisHSMQ.IO
import RedisHSMQ.Types

import qualified System.Logger as Logger

main :: IO ()
main = do
    g <- Logger.new Logger.defSettings
    p <- mkPool g (setHost "localhost" defSettings)
    runRedis p $ commands $ enqueue dummyQueue >> empty
  where
    dummyQueue = QueueName "dummy"

    empty =
        pop dummyQueue >>= \case
            Just v  -> liftIO (print v) >> empty
            Nothing -> return ()
