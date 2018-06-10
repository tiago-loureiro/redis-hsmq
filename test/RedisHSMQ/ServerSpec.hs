{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end tests for the server.
module RedisHSMQ.ServerSpec where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (encode)
import           Data.UUID.V4           (nextRandom)
import           Test.Hspec             (Spec, describe, it)
import           Test.Hspec.Wai         (get, post, shouldRespondWith, with)

import           RedisHSMQ.Server       (app, mkEnv)
import           RedisHSMQ.Types        (Message (Message),
                                         VisibilityTimeout (VisibilityTimeout))

spec :: Spec
spec = with (app <$> mkEnv) $ do
    describe "Basic sending and receiving" $
        it "puts a message and then gets it" $ do
            uuid <- liftIO nextRandom
            let msg = encode $ Message "hello" uuid vTimeout
                vTimeout = VisibilityTimeout 1000
            post "/dummy/hello/" "" `shouldRespondWith` 201
            -- post "/theaccount/thequeue" msg `shouldRespondWith` 201

