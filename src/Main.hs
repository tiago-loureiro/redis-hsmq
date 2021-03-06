{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Database.Redis.IO
import RedisHSMQ.IO
import RedisHSMQ.Server
import RedisHSMQ.Monitor
import RedisHSMQ.Types as RT

import qualified Control.Concurrent.Async as Async
import qualified Data.UUID as UUID

main :: IO ()
main = do
    e <- mkEnv
    m <- Async.async $ runMonitor (redis e)
    _ <- forkIO $ runRandomClient (redis e)
    startServer e `finally` do
        Async.cancel m
        stopServer e
    print ("Exited!" :: String)

runRandomClient :: Pool -> IO ()
runRandomClient p = do
    let timeout = VisibilityTimeout 20
    ret <- try $ runRedis p $ commands $ do
                _ <- enqueue dummyQueue (RT.Message "foo" UUID.nil timeout)
                _ <- enqueue dummyQueue (RT.Message "foo" UUID.nil timeout)
                emptyQueue
    case ret of
        Left  e -> print (e :: SomeException)
        Right r -> print r
    runRedis p $ commands $ inProcess >>= liftIO . print
  where
    dummyQueue = QueueName "dummy"

    emptyQueue = do
        val <- pop' dummyQueue (Seconds 1) (VisibilityTimeout 5)
        case val of
            (Just v) -> liftIO (print v) >> emptyQueue
            Nothing  -> return ()
