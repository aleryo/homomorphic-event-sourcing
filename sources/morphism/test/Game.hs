{-# LANGUAGE DeriveDataTypeable #-}
module Game where

import           Data.Data

data Game = Game { numPlayers :: Int, numRobots :: Int }
  deriving (Eq, Show, Data, Typeable)

data Front = SetPlayerName
           | RegisterPlayer
           | SetNumRobots
           | SetNumPlayers
           | CreateGame
  deriving (Eq, Show, Data, Typeable)

data Back = List | NewGame
  deriving (Eq, Show, Data, Typeable)
