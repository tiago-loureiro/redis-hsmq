{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Database.Redis.IO
import RedisHSMQ.IO
import RedisHSMQ.Types as RT

import qualified System.Logger as Logger

main :: IO ()
main = do
    g <- Logger.new Logger.defSettings
    p <- mkPool g (setHost "localhost" defSettings)
    runRedis p $ commands $ do
        enqueue dummyQueue (RT.Message "foo" "bar")
        enqueue dummyQueue (RT.Message "foo" "baz")
        empty
  where
    dummyQueue = QueueName "dummy"

    empty =
        pop dummyQueue >>= \case
            Just v  -> liftIO (print v) >> empty
            Nothing -> return ()
