{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PetStore.Messages where

import           Data.Aeson
import           GHC.Generics

data PetType = Cat | Dog | Canary | Fish | Rabbit
  deriving (Eq, Show, Enum,Generic,ToJSON,FromJSON)

data Pet = Pet { petName :: String
               , petType :: PetType
               }
           deriving (Eq,Show,Generic,ToJSON,FromJSON)

-- should be Command
data Input = Add { pet :: Pet }
           | Remove { pet :: Pet }
           -- level2: Checkout
           -- level3: AddAccessory/RemoveAccessory w/ constraints depending on type of pet
           | ListPets
           deriving (Eq, Show,Generic,ToJSON,FromJSON)

-- should be Event
data Output = PetAdded { pet :: Pet }
            | PetRemoved { pet :: Pet }
            | Pets { pets :: [ Pet ] }
            | Error { reason :: PetStoreError }
           deriving (Eq, Show,Generic,ToJSON,FromJSON)

-- some errors
data PetStoreError = PetAlreadyAdded
                   | PetDoesNotExist
           deriving (Eq, Show,Generic,ToJSON,FromJSON)
