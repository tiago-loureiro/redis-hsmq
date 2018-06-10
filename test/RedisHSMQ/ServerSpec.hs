-- | End-to-end tests for the server.
module RedisHSMQ.ServerSpec where

import           Network.Wai      (Application)
import           Test.Hspec       (Spec, it, pending)

import           RedisHSMQ.Server (app, mkEnv)

app :: IO Application
app = undefined

spec :: Spec
spec =
    it "Basic sending and receiving" pending

