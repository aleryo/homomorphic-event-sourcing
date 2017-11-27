{-# LANGUAGE MultiParamTypeClasses #-}
{- | A specificaction of `Acquire.Game` interactions in terms of `IOAutomaton`

'''Work in progress'''

Model is currently incomplete:

 * does not handle proper `GameId` generation, requires setting `StdGen` at start and
   passing it down to actual interpreter
 * does not handle game play

-}
module AcquireSpec where

import           Acquire.Game hiding (GameState, Message, newGame)
import           Acquire.Net  (GameDescription (..), Result)
import qualified Acquire.Net  as Net
import           IOAutomaton
import           Messages

data GameState = GameState { acquireState :: AcquireState
                           , gameState    :: Maybe GameDescription
                           }
  deriving (Eq, Show, Read)

data AcquireState = Init
                  | GameCreated
                  | GameStarted
                  | GameEnded
                  | Sink
                  deriving (Eq, Show, Read)

instance IOAutomaton GameState AcquireState Message Result where
  init       = GameState Init Nothing
  sink       = const Sink
  state      = acquireState
  update a q = a { acquireState = q }
  action     = acquire

acquire :: Message -> GameState -> (Maybe Result, GameState)
acquire CreateGame{..} = newGame numHumans numRobots
acquire List           = list
acquire JoinGame{..}   = joinGame playerName gameId
acquire Action{..}     = playAction selectedPlay
acquire Bye            = \ (GameState _state _) -> (Nothing, GameState GameEnded Nothing)

newGame :: Int -> Int ->  GameState -> (Maybe Result, GameState)
newGame h r (GameState Init _) = (Just $ Net.NewGameCreated "12345678", GameState GameCreated (Just $ GameDescription "12345678" h r [] False))
  -- dummy GameId -> inject stdgen to generate a valid game id
newGame _ _ g                  = (Nothing, g)

list :: GameState -> (Maybe Result, GameState)
list curState@(GameState _state (Just gstate)) = (Just $ Net.GamesListed [ gstate ], curState)
list curState@(GameState _state Nothing)       = (Just $ Net.GamesListed [ ], curState)

joinGame :: PlayerName -> GameId -> GameState -> (Maybe Result, GameState)
joinGame _pname _gid (GameState GameCreated (Just desc @ (GameDescription "12345678" 1 _robots [] False)))
  = (Just $ Net.GameStarted "12345678", GameState GameStarted (Just $ desc { descLive = True } ))
joinGame _     _   g = (Nothing, g)

playAction :: Int -> GameState -> (Maybe Result, GameState)
playAction _actionNum (GameState GameStarted (Just desc @ (GameDescription "12345678" 1 _ [] False)))
  = (Just $ Net.ErrorMessage "unsupported action pending model completion -- should be Played xxx", GameState GameStarted (Just $ desc { descLive = True } ))
playAction _         g = (Nothing, g)
