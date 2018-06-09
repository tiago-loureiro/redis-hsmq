{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Database.Redis.IO
import RedisHSMQ.IO
import RedisHSMQ.Types as RT

import qualified System.Logger as Logger

main :: IO ()
main = do
    g <- Logger.new Logger.defSettings
    p <- mkPool g (setHost "localhost" defSettings)
    ret <- try $ runRedis p $ commands $ do
            enqueue dummyQueue (RT.Message "foo" "bar")
            enqueue dummyQueue (RT.Message "foo" "baz")
            empty
    case ret of
        Left e  -> print (e :: SomeException)
        Right r -> print r
    runRedis p $ commands $ do
        liftIO $ print "In process: "
        inProcess >>= liftIO . print
  where
    dummyQueue = QueueName "dummy"

    empty = do
        val <- pop' dummyQueue (Seconds 1)
        case val of
            (Just v) -> liftIO (print v) >> empty
            Nothing  -> return ()
