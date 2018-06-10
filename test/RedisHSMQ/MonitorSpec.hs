{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RedisHSMQ.MonitorSpec (spec) where

import Database.Redis.IO
import Data.Int
import Test.Hspec

import qualified System.Logger as Logger


spec :: Spec
spec = describe "Monitor" $ do
  it "talks" $ do
    lgr <- Logger.new Logger.defSettings
    pool <- mkPool lgr (setHost "localhost" defSettings)
    resp :: Int64 <- runRedis pool . commands $ lpush "Q" ((1 :: Int) :| [2..10])
    msgs :: [Int] <- runRedis pool . commands $ lrange "Q" 0 (-1)
    resp `shouldSatisfy` (>= 10)
    take 10 (reverse msgs) `shouldBe` [1..10]
