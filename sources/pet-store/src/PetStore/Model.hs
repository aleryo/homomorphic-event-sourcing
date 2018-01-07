{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module PetStore.Model where

import           Data.List   (delete)
import           IOAutomaton

data Input = CheckIn { petName :: String }
           | CheckOut { petName :: String }
           | ListPets
           deriving (Eq, Show)

data Output = PetCheckedIn { petName :: String }
            | PetCheckedOut { petName :: String }
            | Pets { pets :: [ String ] }
            | Error { reason :: PetStoreError }
           deriving (Eq, Show)

data PetStoreError = PetAlreadyCheckedIn
                   | PetAlreadyCheckedOut
           deriving (Eq, Show)

-- | Concrete state of a `PetStore`
newtype PetStore = PetStore { hostedPets :: [ String ] }
                 deriving (Eq, Show)

-- | Abstract state of a `PetStore`
-- It is either @Open@ or invalid @Sink@
data PetStoreState = PetStoreOpen
                   | Sink
                     -- ^Special /sink/ state where all unhandled transitions endup
                   deriving (Eq, Show)

petStore :: Input
         -> PetStore
         -> (Maybe Output, PetStore)

petStore CheckIn{petName}  store@PetStore{hostedPets}
  | petName `notElem` hostedPets = (Just $ PetCheckedIn petName, PetStore $ petName:hostedPets)
  | otherwise                    = (Just $ Error PetAlreadyCheckedIn, store)

petStore CheckOut{petName}  store@PetStore{hostedPets}
  | petName `notElem` hostedPets = (Just $ Error PetAlreadyCheckedOut, store)
  | otherwise                    = (Just $ PetCheckedOut petName, PetStore $ delete petName hostedPets)

petStore ListPets          s@PetStore{hostedPets} = (Just $ Pets hostedPets, s)

instance IOAutomaton PetStore PetStoreState Input Output where
  init       = PetStore []
  sink       = const Sink
  state      = const PetStoreOpen
  update a _ = a
  action     = petStore
