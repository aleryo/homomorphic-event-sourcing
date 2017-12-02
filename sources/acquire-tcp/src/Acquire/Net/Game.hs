{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net.Game where

import           Acquire.Net.Types
import           Network.Socket
import           System.IO

connectTo :: String -> PortNumber -> IO (AddrInfo, Handle)
connectTo host port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  return (server,h)

runNewGame :: String -> PortNumber -> Int -> Int -> IO Result
runNewGame host port numHumans numRobots = do
  h <- snd <$> connectTo host port
  let command = CreateGame numHumans numRobots
  hPrint h command
  res :: Result <- read `fmap` hGetLine h
  hClose h
  return res

listGames :: String -> PortNumber -> IO Result
listGames host port = do
  h <- snd <$> connectTo host port
  hPrint h ListGames
  res :: Result <- read `fmap` hGetLine h
  hClose h
  return res


joinGame :: String -> PortNumber -> String -> GameId -> IO Result
joinGame host port player gameId = do
  h <- snd <$> connectTo host port
  hPrint h $ JoinGame player gameId
  res :: Result <- read `fmap` hGetLine h
  hClose h
  return res
