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

import           Acquire.Game             (Game, GameId, PlayerName)
import qualified Acquire.Game             as Game
import           Acquire.Messages
import           Acquire.Net              (GameDescription (..), Result)
import qualified Acquire.Net              as Net
import           Control.Concurrent.Async (Async, cancel)
import           Control.Exception        (ErrorCall, catch, evaluate, throw)
import           Control.Monad.State
import           Data.Monoid              ((<>))
import           Data.Typeable
import           IOAutomaton
import           Network.Socket           (socketPort)
import           System.IO                (Handle, hGetLine, hPrint)
import           System.Random            (StdGen, mkStdGen)

data GameState = GameState { acquireState :: AcquireState
                           , gameState    :: Maybe GameDescription
                           , theGame      :: Maybe Game
                           , randomSeed   :: StdGen
                           }
  deriving (Eq, Show)

-- | Type of /input/ letters
-- An `Input` is either some `Message` from the /player/ to the /game/ or no message, which
-- is denoted by `Eps`ilon.
data Input = Msg Message
           | Eps
  deriving (Eq, Show)

data AcquireState = Init
                  | GameCreated
                  | GameStarted
                  | PlayingPlayer Int
                  | GameEnded
                  | Sink
                  deriving (Eq, Show, Read)

instance IOAutomaton GameState AcquireState Input Result where
  init       = GameState Init Nothing Nothing seed
  sink       = const Sink
  state      = acquireState
  update a q = a { acquireState = q }
  action     = acquire

-- | A fixed initial seed for random numbers
-- This allows a purely deterministic game
seed :: StdGen
seed = mkStdGen 42

instance Eq StdGen where
  s == s' = show s == show s'

startServer :: IO (Async (), Net.PortNumber)
startServer = do
  (s,t) <- Net.runServer 0 seed
  (t, ) <$> socketPort s

stopServer :: (Async (), Net.PortNumber) -> IO ()
stopServer (thread, _) = cancel thread

instance Interpreter (StateT (String, Net.PortNumber, Maybe Handle) IO) GameState AcquireState Input Result where
  interpret _currentState (Msg ListGames)      = get >>= \ (h,p,_) -> liftIO $ Just <$> Net.listGames h p
  interpret _             (Msg CreateGame{..}) = get >>= \ (h,p,_) -> liftIO $ Just <$> Net.runNewGame h p numHumans numRobots
  interpret _             (Msg JoinGame{..})   = get >>= runGame
    where
      runGame (host,port,_) = do
        (_, h) <- liftIO $ Net.connectTo host port
        liftIO $ hPrint h (Net.JoinGame playerName gameId)
        res <- readOrThrow (Proxy :: Proxy Net.Result) h
        put (host,port,Just h)
        msg :: Game.Message <- readOrThrow (Proxy :: Proxy Game.Message) h
        liftIO $ putStrLn $ "received "  <> show msg
        return $ Just res

  interpret _             _              = pure Nothing

  before _ = pure ()
  after _  = pure ()

readOrThrow :: (MonadIO m, Read a, Typeable a) => Proxy a -> Handle -> m a
readOrThrow p hdl = liftIO $ do
  string <- hGetLine hdl
  evaluate (read string)
    `catch` \ (e :: ErrorCall) -> putStrLn ("fail to read value " <> show (typeRep p) <> " from " <> show string) >> throw e

instance Inputs GameState Input where
  inputs (GameState Init _ _ _)                                              = Msg <$> [ CreateGame 1 5, ListGames ]
  inputs (GameState GameCreated (Just GameDescription{gameDescId}) _ _)      = Msg <$> [ JoinGame "player1" gameDescId, ListGames ]
  inputs (GameState GameStarted (Just GameDescription{descLive = True}) _ _) = Msg <$> [ Action 1, ListGames ]
  inputs _                                                                   = []

acquire :: Input -> GameState -> (Maybe Result, GameState)
acquire (Msg CreateGame{..} ) = createGame numHumans numRobots
acquire (Msg ListGames      ) = list
acquire (Msg JoinGame{..}   ) = joinGame playerName gameId
acquire (Msg Action{..}     ) = playAction selectedPlay
acquire (Msg Bye            ) = \ (GameState _state _ _ s) -> (Nothing, GameState GameEnded Nothing Nothing s)
                                                     -- missing a result to indicate termination of game? -> we need a fully initiated game
acquire Eps                   = (Nothing,)

createGame :: Int -> Int -> GameState -> (Maybe Result, GameState)
createGame h r (GameState Init _ _ s) =
  let nextId = Net.randomGameId s
  in (Just $ Net.NewGameCreated nextId, GameState GameCreated (Just $ GameDescription nextId h r [] False) Nothing s)
createGame _ _ g                  = (Nothing, g)

list :: GameState -> (Maybe Result, GameState)
list curState@(GameState _state (Just gstate) _ _seed) = (Just $ Net.GamesListed [ gstate ], curState)
list curState@(GameState _state Nothing _ _seed)       = (Just $ Net.GamesListed [ ], curState)

joinGame :: PlayerName -> GameId -> GameState -> (Maybe Result, GameState)
joinGame _pname gid (GameState GameCreated (Just desc @ (GameDescription _descId 1 _robots [] False)) _game _seed)
  = (Just $ Net.GameStarted gid, GameState GameStarted (Just $ desc { descLive = True } ) _game _seed)
joinGame _     _   g = (Nothing, g)

playAction :: Int -> GameState -> (Maybe Result, GameState)
playAction _actionNum (GameState GameStarted (Just desc @ (GameDescription _descId 1 _ [] False)) _game ranSeed)
  = (Just $ Net.ErrorMessage "unsupported action pending model completion -- should be Played xxx", GameState GameStarted (Just $ desc { descLive = True }) _game ranSeed)
playAction _         g = (Nothing, g)
