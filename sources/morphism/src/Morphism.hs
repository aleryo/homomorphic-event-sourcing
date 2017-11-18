{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Morphism where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


newtype Morphism a b = Morphism { transitions :: [ Transition a b ] }
  deriving (Eq, Show, Read)

morph :: Morphism a b -> [ a ] -> [ b ]
morph (Morphism _trs) _inword = undefined

data Transition a b = Tr { fromState    :: State
                         , sourceLetter :: Input a
                         , targetLetter :: Out b
                         , toState      :: State
                         }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

newtype Input a = In a
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

newtype State = State Int
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Out a = Eps
           | Com a
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
