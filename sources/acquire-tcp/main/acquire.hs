{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Acquire.Game
import           Acquire.Net
import           Acquire.Pretty           hiding ((<$>), (<>))
import           Acquire.Trace
import           Control.Concurrent.Async (wait)
import           Data.Monoid
import           Network.Socket           (socketPort)
import           Options.Applicative

data Configuration = Server { serverPort :: PortNumber }
                   | ClientPlayer { serverHost :: String
                                  , serverPort :: PortNumber
                                  , playerName :: PlayerName
                                  , playGameId :: GameId
                                  , playerType :: PlayerType
                                  }
                   | ClientNewGame { serverHost           :: String
                                   , serverPort           :: PortNumber
                                   , numberOfHumanPlayers :: Int
                                   , numberOfRobotPlayers :: Int
                                   }
                   | ClientListGames { serverHost :: String
                                     , serverPort :: PortNumber
                                     } deriving (Show)

configOptions :: Parser Configuration
configOptions = subparser
                ( command "server" (info serverOptions
                                    (progDesc "run an Acquire server instance listening for clients on a given port. Game starts when 6 players are connected."))
                  <> ( command "player" (info clientPlayerOptions
                                         (progDesc "run an Acquire client to connect to a given server and play as Human")))
                  <> ( command "newGame" (info newGameOptions
                                          (progDesc "connect to a server and request to start a fresh new game, returns id of the game")))
                  <> ( command "list" (info listGamesOptions
                                          (progDesc "connect to a server and list all active games"))))

portOption :: String -> Parser PortNumber
portOption hlp = option (read <$> str)
                 ( long "port"
                   <> short 'p'
                   <> value 7890
                   <> metavar "PORT"
                   <> help hlp)

serverOptions :: Parser Configuration
serverOptions = Server <$> portOption "Port to listen for client connections"

clientPlayerOptions :: Parser Configuration
clientPlayerOptions = ClientPlayer
                <$> strOption ( long "host"
                                <> short 'h'
                                <> value "localhost"
                                <> metavar "HOST"
                                <> help "Server host to connect to" )
                <*> portOption "Server port to connect to"
                <*> strOption ( long "player"
                                <> short 'n'
                                <> metavar "NAME"
                                <> help "Player name (must be unique for a game)" )
                <*> strOption ( long "game"
                                <> short 'g'
                                <> metavar "GAME ID"
                                <> help "Game id to connect to (set with 'newGame')" )
                <*> option auto ( long "player-type"
                                  <> short 't'
                                  <> value Human
                                  <> metavar "PLAYER-TYPE"
                                  <> help "Player type: Human or Robot" )

newGameOptions :: Parser Configuration
newGameOptions = ClientNewGame
                <$> strOption ( long "host"
                                <> short 'h'
                                <> value "localhost"
                                <> metavar "HOST"
                                <> help "Server host to connect to" )
                <*> portOption "Server port to connect to"
                <*> option auto ( long "num-humans"
                                <> short 'H'
                                <> metavar "NUMBER"
                                <> value 1
                                <> help "Number of human players")
                <*> option auto ( long "num-robots"
                                <> short 'R'
                                <> metavar "NUMBER"
                                <> value 5
                                <> help "Number of robot players")


listGamesOptions :: Parser Configuration
listGamesOptions = ClientListGames
                <$> strOption ( long "host"
                                <> short 'h'
                                <> value "localhost"
                                <> metavar "HOST"
                                <> help "Server host to connect to" )
                <*> portOption "Server port to connect to"

start :: Configuration -> IO ()
start Server{..}          = do
  (p, t) <- runServer serverPort
  socketPort p >>= trace . ("Server started on port " ++) . show
  wait t
start ClientPlayer{..}    = runPlayer  serverHost serverPort playerName playGameId consoleIO
start ClientNewGame{..}   = do
  res <- runNewGame serverHost serverPort numberOfHumanPlayers numberOfRobotPlayers
  putDoc $ pretty res
  putStrLn ""
start ClientListGames{..} = do
  r <- listGames  serverHost serverPort
  putDoc $ pretty r
  putStrLn ""



main :: IO ()
main = execParser opts >>= start
  where
    opts = info (helper <*> configOptions)
      ( fullDesc
        <> progDesc "Run an Acquire game in client or server mode"
        <> header "Acquire - A Game on Investment" )
