{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RedisHSMQ.MonitorSpec (spec) where

import Control.Monad.IO.Class
import Database.Redis.IO hiding (Message)
import Data.ByteString.Conversion
import Data.Int
import Data.String.Conversions
import Data.Time
import RedisHSMQ.Monitor
import RedisHSMQ.Types
import Test.Hspec

-- import qualified Database.Redis.IO as RedisIO
import qualified Data.List.NonEmpty as NEL
import qualified Data.UUID as UUID
import qualified System.Logger as Logger


run :: MonadIO m => Pool -> Redis IO a -> m a
run pool = runRedis pool . commands

-- | all the keys potentially used in these tests.
queues :: NonEmpty Key
queues = "Q" :| ["PQ"]

timeNow :: UTCTime
Just timeNow = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2018-06-10T15:32:18Z"

timeTomorrow :: UTCTime
Just timeTomorrow = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2018-06-11T15:32:18Z"

_timeYesterday :: UTCTime
Just _timeYesterday = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2018-06-09T15:32:18Z"


redis :: IO Pool
redis = do
  lgr <- Logger.new Logger.defSettings
  pool <- mkPool lgr (setHost "localhost" defSettings)
  _ <- run pool $ del queues
  pure pool


spec :: Spec
spec = describe "Monitor" . before redis $ do
  it "talks" $ \pool -> do
    resp :: Int64 <- run pool $ lpush "Q" ((1 :: Int) :| [2..10])
    msgs :: [Int] <- run pool $ lrange "Q" 0 (-1)
    resp `shouldSatisfy` (>= 10)
    take 10 (reverse msgs) `shouldBe` [1..10]

  describe "monitorLoop" $ do
    let launch pool = run pool $ monitorLoop (MonTimeout 3 20) timeNow

    context "PQ=[]" $ do
      it "does not change PQ" $ \pool -> do
        () <- launch pool
        shouldBeQueueWith @Message pool "PQ" []

      it "does not change Q" $ \pool -> do
        () <- launch pool
        shouldBeQueueWith @Message pool "Q" []

    context "PQ=[fresh]" $ do
      let qcontents = Message "whee" UUID.nil (VisibilityTimeout 20) :| []
          loadqcontents pool = do
            _ <- run pool $ lpush "PQ" qcontents
            _ <- run pool $ set (Key . cs . UUID.toString $ UUID.nil) (EndOfLife timeTomorrow) nx
            pure ()

      it "does not change PQ" $ \pool -> do
        loadqcontents pool
        launch pool
        shouldBeQueueWith @Message pool "PQ" (NEL.toList qcontents)

      it "does not change Q" $ \pool -> do
        loadqcontents pool
        launch pool
        shouldBeQueueWith @Message pool "Q" []

    context "PQ=[stale]" $ do
      pure ()

    context "PQ=[stale,fresh,stale,stale]" $ do
      pure ()


shouldBeQueueWith :: forall a. (Eq a, Show a, FromByteString a) => Pool -> Key -> [a] -> Expectation
shouldBeQueueWith pool qname qcontents =
  run pool (lrange @IO @a qname 0 (-1)) `shouldReturn` qcontents
