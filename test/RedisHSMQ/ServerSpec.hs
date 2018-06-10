{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end tests for the server.
module RedisHSMQ.ServerSpec where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (encode)
import           Data.UUID.V4              (nextRandom)
import           Network.HTTP.Types.Method (methodPost)
import           System.Process            (callCommand)
import           Test.Hspec                (Spec, beforeAll, describe, it)
import           Test.Hspec.Wai            (get, post, request,
                                            shouldRespondWith, with)

import           RedisHSMQ.Server          (app, mkEnv)
import           RedisHSMQ.Types           (Message (Message), VisibilityTimeout (VisibilityTimeout))

-- TODO: send a PR to `Test.Hspec.Wai`
postJson path contents = request methodPost path [("Content-Type", "application/json")] contents

spec :: Spec
spec = beforeAll startRedis $ do
    with (app <$> mkEnv) $ do
        describe "Basic sending and receiving" $
            it "puts a message and then gets it" $ do
            uuid <- liftIO nextRandom
            let msg = encode $ Message "hello" uuid vTimeout
                vTimeout = VisibilityTimeout 1000
            postJson "/theaccount/thequeue/"  msg `shouldRespondWith` 201
    where
      startRedis =
          callCommand "docker run --rm --net=host -d redis:alpine"

