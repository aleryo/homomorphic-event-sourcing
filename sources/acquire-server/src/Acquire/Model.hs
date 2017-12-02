{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{- | A specificaction of `Acquire.Game` interactions in terms of `IOAutomaton`

'''Work in progress'''

Model is currently incomplete:

 * does not handle proper `GameId` generation, requires setting `StdGen` at start and
   passing it down to actual interpreter
 * does not handle game play

-}
module Acquire.Model where

import           Acquire.Game             hiding (GameState, Message)
import           Acquire.Messages
import           Acquire.Net              (GameDescription (..), Result)
import qualified Acquire.Net              as Net
import           Control.Concurrent.Async (Async, cancel)
import           Control.Exception        (ErrorCall, catch, evaluate, throw)
import           Control.Monad.State
import           Data.Monoid              ((<>))
import           IOAutomaton
import           Network.Socket           (socketPort)
import           System.IO                (Handle, hGetLine, hPrint)
import           System.Random            (StdGen, mkStdGen)

data GameState = GameState { acquireState :: AcquireState
                           , gameState    :: Maybe GameDescription
                           , randomSeed   :: StdGen
                           }
  deriving (Eq, Show)

instance Eq StdGen where
  s == s' = show s == show s'

data AcquireState = Init
                  | GameCreated
                  | GameStarted
                  | GameEnded
                  | Sink
                  deriving (Eq, Show, Read)

instance IOAutomaton GameState AcquireState Message Result where
  init       = GameState Init Nothing seed
  sink       = const Sink
  state      = acquireState
  update a q = a { acquireState = q }
  action     = acquire

    -- startGame _p playerName gameId = do
    --   (w,r)   <- newChan
    --   (w',r') <- newChan

    --   -- we run 2 asyncs, one for handling player commands and general game play,
    --   -- the other to pump server's response to WS connection. This seems necessary because
    --   -- we have 2 connections to handle:
    --   --
    --   --  * WS Connection between remote client's UI and this server code,
    --   --  * Chan-based connection between player's proxy and main server
    --   --
    --   -- There should be a way to greatly simplify this code using directly pure version of the game
    --   -- instead of wrapping the CLI server.
    --   toServer <- async $ do
    --     trace $ "starting game loop for player " ++ playerName ++ " @" ++ gameId
    --     runPlayer "localhost" p playerName gameId (io (w,r'))
    --     trace $ "stopping game loop for player " ++ playerName ++ " @" ++ gameId

    --   toClient <- async $ do
    --     trace $ "starting response sender for player " ++ playerName ++ " @" ++ gameId
    --     forever $ do
    --       v <- readChan r
    --       cnx <- clientConnection <$> readIORef channels
    --       sendTextData cnx v
    --         `catch` (\ (e :: ConnectionException) -> trace $ "response sender error: " ++ (show e))

    --   -- we set the write channel to the other end of the pipe used by player loop for
    --   -- reading. This channel will be used by subsequent commands sent by client and
    --   -- "pumped" to server
    --   modifyIORef channels ( \ c -> c { inChan = w'
    --                                   , serverPump = Just toServer, clientPump = Just toClient })

    -- cleanup = do
    --   ClientConnection _w _r cnx sp cp <- readIORef channels
    --   sendClose cnx ("Bye" :: Text)
    --   maybe (return ()) cancel sp
    --   maybe (return ()) cancel cp
    --   modifyIORef channels ( \ c -> c { serverPump = Nothing, clientPump = Nothing })

    -- handleCommand List = do
    --   r <- listGames "localhost" p
    --   sendTextData connection (encode r)
    -- handleCommand (CreateGame numHumans numRobots) = do
    --   r <- runNewGame "localhost" p numHumans numRobots
    --   sendTextData connection (encode r)
    -- handleCommand (JoinGame playerName gameId) =
    --   startGame p playerName gameId
    -- handleCommand (Action n) = do
    --   w <- inChan <$> readIORef channels
    --   writeChan w (show n)
    --   trace $ "action " ++ show n
    -- handleCommand Bye = sendClose connection ("Bye" :: Text)

seed :: StdGen
seed = mkStdGen 42

startServer :: IO (Async (), Net.PortNumber)
startServer = do
  (s,t) <- Net.runServer 0 seed
  (t, ) <$> socketPort s

stopServer :: (Async (), Net.PortNumber) -> IO ()
stopServer (thread, _) = cancel thread

instance Interpreter (StateT (String, Net.PortNumber, Maybe Handle) IO) GameState AcquireState Message Result where
  interpret _currentState List           = get >>= \ (h,p,_) -> liftIO $ Just <$> Net.listGames h p
  interpret _             CreateGame{..} = get >>= \ (h,p,_) -> liftIO $ Just <$> Net.runNewGame h p numHumans numRobots
  interpret _             JoinGame{..}   = get >>= runGame
    where
      runGame (host,port,_) = do
        (_, h) <- liftIO $ Net.connectTo host port
        liftIO $ hPrint h (Net.JoinGame playerName gameId)
        ln <- liftIO $ hGetLine h
        res :: Net.Result <- liftIO $ evaluate (read ln) `catch` \ (e :: ErrorCall) -> putStrLn ("fail to read Result from " <> show ln) >> throw e
        put (host,port,Just h)
        return $ Just res

  interpret _             _              = pure Nothing

  before _ = pure ()
  after _  = pure ()

instance Inputs GameState Message where
  inputs (GameState Init _ _)           = [ CreateGame 1 5, List ]
  inputs (GameState GameCreated (Just GameDescription{gameDescId}) _) = [ JoinGame "player1" gameDescId, List ]
  inputs (GameState GameStarted (Just GameDescription{descLive = True}) _) = [ Action 1, List ]
  inputs _ = []

acquire :: Message -> GameState -> (Maybe Result, GameState)
acquire CreateGame{..} = createGame numHumans numRobots
acquire List           = list
acquire JoinGame{..}   = joinGame playerName gameId
acquire Action{..}     = playAction selectedPlay
acquire Bye            = \ (GameState _state _ s) -> (Nothing, GameState GameEnded Nothing s)
                                                     -- missing a result to indicate termination of game? -> we need a fully initiated game

createGame :: Int -> Int -> GameState -> (Maybe Result, GameState)
createGame h r (GameState Init _ s) =
  let nextId = Net.randomGameId s
  in (Just $ Net.NewGameCreated nextId, GameState GameCreated (Just $ GameDescription nextId h r [] False) s)
createGame _ _ g                  = (Nothing, g)

list :: GameState -> (Maybe Result, GameState)
list curState@(GameState _state (Just gstate) _seed) = (Just $ Net.GamesListed [ gstate ], curState)
list curState@(GameState _state Nothing _seed)       = (Just $ Net.GamesListed [ ], curState)

joinGame :: PlayerName -> GameId -> GameState -> (Maybe Result, GameState)
joinGame _pname gid (GameState GameCreated (Just desc @ (GameDescription _descId 1 _robots [] False)) _seed)
  = (Just $ Net.GameStarted gid, GameState GameStarted (Just $ desc { descLive = True } ) _seed)
joinGame _     _   g = (Nothing, g)

playAction :: Int -> GameState -> (Maybe Result, GameState)
playAction _actionNum (GameState GameStarted (Just desc @ (GameDescription _descId 1 _ [] False)) ranSeed)
  = (Just $ Net.ErrorMessage "unsupported action pending model completion -- should be Played xxx", GameState GameStarted (Just $ desc { descLive = True }) ranSeed)
playAction _         g = (Nothing, g)
