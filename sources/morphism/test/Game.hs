{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Game where

import           Data.Aeson
import           Data.Data
import           GHC.Generics

data Game = Game { numPlayers :: Int, numRobots :: Int }
  deriving (Eq, Show, Data, Typeable)

data Front = SetPlayerName
           | RegisterPlayer
           | SetNumRobots
           | SetNumPlayers
           | CreateGame
  deriving (Eq, Show, Data, Typeable, Generic, ToJSON, FromJSON)

data Back = List | NewGame
  deriving (Eq, Show, Data, Typeable, Generic, ToJSON, FromJSON)
