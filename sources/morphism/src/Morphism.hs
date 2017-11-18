module Morphism where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Morphism a b = Morphism [ Transition a b ]
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Transition a b = Tr { fromState    :: State
                         , sourceLetter :: In a
                         , targetLetter :: Out b
                         , toState      :: State
                         }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

newtype State = State { stateNumber :: Int }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data In a = In a (Args a)
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

newtype Args a = Args [ Arg a ]
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Arg a = Var String AType
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data AType = AString
           | AInt
           | AObject
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Out a = Eps
           | Com a
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
