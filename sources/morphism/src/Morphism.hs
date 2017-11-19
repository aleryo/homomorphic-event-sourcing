{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
module Morphism where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.List    (reverse)
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics (Generic)


newtype Morphism a b = Morphism { transitions :: [ Transition a b ] }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

morph :: forall a b . (Show a, Eq a, Eq b) => Morphism a b -> [ a ] -> [ b ]
morph (Morphism trs) inword = fromOut $ morph' inword [] (State 0)
  where
    morph' :: [ a ] -> [ Out b ] -> State -> [ Out b ]
    morph' []     outword _     = reverse outword
    morph' (w:ws) outword state =
      case lookupTransition state (In w) trs of
        Nothing                       -> error $ "undefined transition for " <> show (state,w)
        Just Tr{targetLetter,toState} -> morph' ws (targetLetter:outword) toState

    lookupTransition state letter =
      listToMaybe . filter (\ Tr{fromState,sourceLetter} -> state == fromState && sourceLetter == letter)

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

fromOut :: (Eq a) => [ Out a ] -> [ a ]
fromOut = fmap unCom . filter (/= Eps)
  where
    unCom (Com l) = l
    unCom _       = undefined
