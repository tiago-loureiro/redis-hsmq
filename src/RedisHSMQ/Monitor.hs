{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module RedisHSMQ.Monitor where  -- TODO: explicit export list.

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Database.Redis.IO
import Data.String.Conversions
import Data.Time
import Data.Void
import RedisHSMQ.Types as RT

import qualified Data.UUID as UUID


queueWait :: Key
queueWait = "Q"

queueInProcess :: Key
queueInProcess = "PQ"

seconds :: Int -> Int
seconds = (* 1000) . (* 1000)

milliseconds :: Int -> Int
milliseconds = (* 1000)

-- | How long between the end of one 'monitorLoop' and the beginning of the next one?
gcFreq :: Int
gcFreq = seconds 5

runMonitor :: Pool -> IO Void
runMonitor pool = do
  tidyUp
  forever $ do
    now <- getCurrentTime
    runRedis pool (commands $ monitorLoop now) >> threadDelay gcFreq

-- | After a crash, go through PQ to eliminate duplicates.
tidyUp :: IO ()
tidyUp = undefined

-- | All 'Message's added to PQ must have fresh 'mId's.
monitorLoop :: forall m. (Monad m, MonadIO m) => UTCTime -> Redis m ()
monitorLoop now = do
  -- read all of PQ into a list.
  msgs :: [Message] <- lrange queueInProcess 0 (-1)
  forM_ msgs $ \msg -> do
    endoflife <- hasTimedOut now msg
    when endoflife $ do
      -- push msg with to end of queue and set it to waiting.
      _ <- rpush queueInProcess (msg :| [])
      -- remove it from its original place.
      _ <- lrem queueInProcess 1 msg
      -- atomically move message back to Q.
      _ <- brpoplpush @m @Message queueInProcess queueWait (Seconds 3)
           -- TODO: see if it worked; if not, either try again or crash.
      pure ()

hasTimedOut :: (Monad m, MonadIO m) => UTCTime -> Message -> Redis m Bool
hasTimedOut now msg = (get . Key . cs . UUID.toString . mId) msg >>= \case
  Just (EndOfLife endOfLife) -> pure $ endOfLife < now  -- TODO: do we need to catch parse errors?
  Nothing -> pure False  -- TODO: retry, timeout.
