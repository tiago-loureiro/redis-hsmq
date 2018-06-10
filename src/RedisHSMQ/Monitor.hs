{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module RedisHSMQ.Monitor
  ( runMonitor

    -- (do not import any of the following except if you are testing.)
  , tidyUp
  , monitorLoop
  , hasTimedOut
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Database.Redis.IO
import Data.Int
import Data.List (foldl')
import Data.String.Conversions
import Data.Time
import Data.UUID (UUID)
import Data.Void
import RedisHSMQ.Types as RT

import qualified Data.Map as Map
import qualified Data.UUID as UUID


----------------------------------------------------------------------
-- config

queueWait :: Key
queueWait = "Q"

queueInProcess :: Key
queueInProcess = "PQ"

-- | How long between the end of one 'monitorLoop' and the beginning of the next one?
monFreq :: Int
monFreq = seconds 5

data MonTimeout = MonTimeout { timeoutRounds :: Int, timeoutRoundLength :: Int }
  deriving (Eq, Show)

monTimeout :: MonTimeout
monTimeout = MonTimeout 10 (milliseconds 300)


----------------------------------------------------------------------
-- helpers

seconds :: Int -> Int
seconds = (* 1000) . (* 1000)

milliseconds :: Int -> Int
milliseconds = (* 1000)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


----------------------------------------------------------------------
-- run

runMonitor :: Pool -> IO Void
runMonitor pool = do
  runRedis pool . commands $ tidyUp
  forever $ do
    now <- getCurrentTime
    runRedis pool (commands $ monitorLoop monTimeout now)
    threadDelay monFreq

-- | After a crash, go through PQ to eliminate duplicates.
--
-- TODO: if there are two messages with the same UUID and different other data, then we're screwed.
-- perhaps it would be better to use Q, PQ, only for storing the UUIDs, and store the full messages
-- under their own UUID as a separate key-value pair.
tidyUp :: forall m. (Monad m, MonadIO m) => Redis m ()
tidyUp = do
  msgs :: [Message] <- lrange queueInProcess 0 (-1)

  let dupes :: [(Message, Int64)]
      dupes = Map.toList
            . Map.filter (> 1)
            . foldr (Map.alter consume) mempty
            $ msgs

      consume :: Maybe Int64 -> Maybe Int64
      consume Nothing = Just 1
      consume (Just i) = Just (i + 1)

  forM_ dupes $ \(msg, numDupes) ->
    lrem queueInProcess numDupes msg

-- | All 'Message's added to PQ must have fresh 'mId's.
monitorLoop :: forall m. (Monad m, MonadIO m) => MonTimeout -> UTCTime -> Redis m ()
monitorLoop timeout now = do
  -- read all of PQ into a list.
  msgs :: [Message] <- lrange queueInProcess 0 (-1)
  forM_ msgs $ \msg -> do
    endoflife <- hasTimedOut timeout now msg
    when endoflife $ do
      -- push msg with to end of queue and set it to waiting.
      _ <- rpush queueInProcess (msg :| [])
      -- remove it from its original place.
      _ <- lrem queueInProcess 1 msg
      -- atomically move message back to Q.
      _ <- brpoplpush @m @Message queueInProcess queueWait (Seconds 3)
           -- TODO: see if it worked; if not, either try again or crash.
      pure ()

hasTimedOut :: (Monad m, MonadIO m) => MonTimeout -> UTCTime -> Message -> Redis m Bool
hasTimedOut (MonTimeout 0 _) _ _ = pure False
hasTimedOut (MonTimeout rounds delay) now msg =
  (get . Key . cs . UUID.toString . mId) msg >>= \case
    Just (EndOfLife endOfLife)
      -> pure $ endOfLife < now  -- TODO: do we need to catch parse errors?
    Nothing
      -> liftIO (threadDelay delay) >>
         hasTimedOut (MonTimeout rounds delay) now msg
