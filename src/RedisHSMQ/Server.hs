{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module RedisHSMQ.Server where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Text (Text)
import Database.Redis.IO
import Network.Wai.Handler.Warp (run)
import Servant

import qualified RedisHSMQ.Types as RT
import qualified RedisHSMQ.IO    as RIO
import qualified System.Logger   as Logger

type GetMessage = Get '[JSON] RT.Message
type AddMessage = ReqBody '[JSON] RT.Message :> PostCreated '[JSON] ()
type AckMessage = DeleteNoContent '[JSON] NoContent -- TODO: Do we really need a type here?
type RedisHSMQAPI = (Capture "accountnr" Text :> Capture "queue" RT.QueueName :> GetMessage) :<|>
                    (Capture "accountnr" Text :> Capture "queue" RT.QueueName :> AddMessage) :<|>
                    (Capture "accountnr" Text :> Capture "queue" RT.QueueName :> Capture "msgid" Text :> AckMessage)

api :: Proxy RedisHSMQAPI
api = Proxy

data State = State
  { redis :: Pool
  }

type AppM = ReaderT State Handler

mkEnv :: IO State
mkEnv = do
    g <- Logger.new Logger.defSettings
    p <- mkPool g (setHost "localhost" defSettings)
    return $ State p

startServer :: State -> IO ()
startServer st = run 8080 $ app $ st

stopServer :: State -> IO ()
stopServer st = shutdown (redis st)

server :: ServerT RedisHSMQAPI AppM
server = getMessage :<|> addMessage :<|> ackMessage
  where
    -- TODO: fisx to add proper implementation
    getMessage :: MonadIO m => Text -> RT.QueueName -> ReaderT State m RT.Message
    getMessage _ qn = do
      State p <- ask
      msg <- runRedis p $ commands $ RIO.pop qn (RT.VisibilityTimeout 5)
      case msg of
        Just m  -> return m
        Nothing -> error "TODO"

    -- TODO: fisx to add proper implementation
    addMessage :: MonadIO m => Text -> RT.QueueName -> RT.Message -> ReaderT State m ()
    addMessage _ qn msg = do
      State p <- ask
      void . runRedis p $ commands
                        $ RIO.enqueue qn
                        $ msg

    ackMessage :: MonadIO m => Text -> RT.QueueName -> Text -> ReaderT State m NoContent
    ackMessage _ _qn _mid = error "fisx to implement"

app :: State -> Application
app s = serve api $ hoistServer api (flip runReaderT s) server
