{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module PetStore.Model where

import           Data.List         (delete)
import           Data.Monoid       ((<>))
import           IOAutomaton
import           PetStore.Messages

-- | Concrete state of a `PetStore`
data PetStore = PetStore { storedPets :: [ Pet ] }
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
  | pet `notElem` storedPets = (Just $ PetAdded pet, store { storedPets = pet:storedPets} )
  | otherwise                = (Just $ Error PetAlreadyAdded, store)

petStore Remove{pet}  store@PetStore{storedPets}
  | pet `notElem` storedPets = (Just $ Error PetDoesNotExist, store)
  | otherwise                = (Just $ PetRemoved pet, store { storedPets = delete pet storedPets } )

-- Actually a Query not a Command
petStore ListPets          s@PetStore{storedPets}
  = (Just $ Pets storedPets, s)


instance IOAutomaton PetStore PetStoreState Input Output where
  init       = PetStore []
  sink       = const Sink
  state      = const PetStoreOpen
  update a _ = a
  action     = petStore

petsNames :: [ String ]
petsNames = [ "Bailey", "Bella", "Max", "Lucy", "Charlie", "Molly", "Buddy", "Daisy", "" ]

instance Inputs PetStore Input where
  inputs (PetStore _) = fmap Add listOfPets <> fmap Remove listOfPets
    where
      listOfPets = [ Pet name species | name <- petsNames, species <- enumFrom Cat ]
