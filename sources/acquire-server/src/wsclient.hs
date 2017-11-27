--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Aeson          (encode)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Messages
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS
import           System.Environment  (getArgs)
import           System.IO


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn (encode Bye)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  [host, port, path] <- getArgs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withSocketsDo $ WS.runClient host (read port) ("/" <> path) app
