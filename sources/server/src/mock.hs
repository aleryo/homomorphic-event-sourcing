{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Acquire.Trace
import           Control.Concurrent.Async       (Async, cancel)
import           Control.Concurrent.Chan.Unagi  (InChan, OutChan, newChan)
import           Control.Concurrent.STM
import           Control.Exception              (catch)
import           Data.Aeson
import qualified Data.ByteString                as SBS
import           Data.Functor
import           Data.IORef
import qualified Data.Map                       as M
import           Data.Monoid                    ((<>))
import           Data.Text.Lazy                 (Text)
import           GHC.Generics
import           Messages
import           Network.HTTP.Types.Status
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.WebSockets             (Connection,
                                                 ConnectionException,
                                                 DataMessage (..),
                                                 PendingConnection,
                                                 RequestHead (..),
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 pendingRequest,
                                                 receiveDataMessage, sendClose,
                                                 sendTextData)
import           System.Environment

newtype CommandError = CommandError { reason :: String }
                     deriving (Generic)

instance ToJSON CommandError

data ClientConnection = ClientConnection { inChan           :: InChan String
                                         , outChan          :: OutChan String
                                         , clientConnection :: Connection
                                         , serverPump       :: Maybe (Async ())
                                         , clientPump       :: Maybe (Async ())
                                         }

handleWS :: TVar (M.Map SBS.ByteString (IORef ClientConnection)) -> PendingConnection -> IO ()
handleWS cnxs pending = do
  let key = requestPath $ pendingRequest pending
  trace $ "got websocket request with key: " ++ show key
  connection <- acceptRequest pending
  ref <- getOrMakeChannels key connection
  handleClient ref connection
  where
    -- This code is unfortunately rather complicated because
    -- we are storing connections and channels mapping client WS connections, based
    -- on path requested. The idea is that the client is supposed to generate random
    -- strings upon first connection so that channels can later be reused when game
    -- is in play, even if it is disconnected. This happens due to timeouts on both
    -- client side and server-side apparently.
    getOrMakeChannels key connection = do
        maybeRef <- atomically $ M.lookup key <$> readTVar cnxs
        case maybeRef of
          Nothing -> do
            (i,o) <- newChan
            let cc = ClientConnection i o connection Nothing Nothing
            ref   <- newIORef cc
            atomically $ modifyTVar cnxs (M.insert key ref)
            trace $ "registering new channels with key " ++ show key
            return ref
          Just r -> do
            modifyIORef r (\ c -> c { clientConnection = connection })
            trace $ "reusing old channels with key " ++ show key
            return r


handleClient :: IORef ClientConnection -> Connection ->  IO ()
handleClient channels connection =
  let clientLoop = do
        Text message _ <- receiveDataMessage connection
        trace $ "received message: " ++ show message
        case eitherDecode message of
          Left e  -> sendTextData connection (encode $ CommandError e)
          Right c -> handleCommand c
        clientLoop

  in clientLoop `catch` (\ (e :: ConnectionException) -> do
                            trace $ "client error: " <> show e <>", closing everything"
                            cleanup)

  where

    cleanup = do
      ClientConnection _w _r cnx sp cp <- readIORef channels
      sendClose cnx ("Bye" :: Text)
      maybe (return ()) cancel sp
      maybe (return ()) cancel cp
      modifyIORef channels ( \ c -> c { serverPump = Nothing, clientPump = Nothing })

    notHandled c = sendTextData connection (encode $ CommandError $ "command " <> show c <> " not handled")

    handleCommand Bye = sendClose connection ("Bye" :: Text)
    handleCommand c   = notHandled c

main :: IO ()
main = do
  [port] <- getArgs
  cnxs <- newTVarIO M.empty
  trace $ "starting mock WS on port " <> port
  void $ run (read port) (WaiWS.websocketsOr defaultConnectionOptions (handleWS cnxs) defaultResponse)
    where
      defaultResponse :: Application
      defaultResponse _ respond = respond $ responseLBS status400 [] "Not a websocket connection!"
