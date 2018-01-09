{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module PetStore.Model where

import           Data.List   (delete)
import           Data.Monoid ((<>))
import           IOAutomaton

data PetType = Cat | Dog | Canary | Fish | Rabbit
  deriving (Eq, Show, Enum)

data Pet = Pet { petName :: String
               , petType :: PetType
               }
           deriving (Eq,Show)

-- should be Command
data Input = Add { pet :: Pet }
           | Remove { pet :: Pet }
           -- level2 : Checkout
           -- level3: AddAccessory/RemoveAccessory w/ constraints depending on type of pet
           | ListPets
           deriving (Eq, Show)

-- should be Event
data Output = PetAdded { pet :: Pet }
            | PetRemoved { pet :: Pet }
            | Pets { pets :: [ Pet ] }
            | Error { reason :: PetStoreError }
           deriving (Eq, Show)

-- some errors
data PetStoreError = PetAlreadyAdded
                   | PetDoesNotExist
           deriving (Eq, Show)

-- | Concrete state of a `PetStore`
newtype PetStore = PetStore { storedPets :: [ Pet ] }
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

petStore Add{pet}  store@PetStore{storedPets}
  | pet `notElem` storedPets = (Just $ PetAdded pet, PetStore $ pet:storedPets)
  | otherwise                = (Just $ Error PetAlreadyAdded, store)

petStore Remove{pet}  store@PetStore{storedPets}
  | pet `notElem` storedPets = (Just $ Error PetDoesNotExist, store)
  | otherwise                = (Just $ PetRemoved pet, PetStore $ delete pet storedPets)

petStore ListPets          s@PetStore{storedPets}
  = (Just $ Pets storedPets, s)


instance IOAutomaton PetStore PetStoreState Input Output where
  init       = PetStore []
  sink       = const Sink
  state      = const PetStoreOpen
  update a _ = a
  action     = petStore

petsNames :: [ String ]
petsNames = [ "Bailey", "Bella", "Max", "Lucy", "Charlie", "Molly", "Buddy", "Daisy" ]

instance Inputs PetStore Input where
  inputs (PetStore _)         = fmap Add listOfPets <> fmap Remove listOfPets
    where
      listOfPets = [ Pet name species | name <- petsNames, species <- enumFrom Cat ]
