{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PetStore.Messages where

import           Data.Aeson
import           GHC.Generics

data PetType = Cat | Dog | Canary | Fish | Rabbit
  deriving (Eq, Show, Enum,Generic,ToJSON,FromJSON)

data Pet = Pet { petName  :: String
               , petType  :: PetType
               , petPrice :: Integer
               }

           deriving (Eq,Show,Generic,ToJSON,FromJSON)

data User = User { userName :: String }
          deriving (Eq,Show,Generic,ToJSON,FromJSON)

data Payment = Payment { amount             :: Integer
                       , paymentInformation :: String
                       }
             deriving (Eq,Show,Generic,ToJSON,FromJSON)

-- rename back to Input / Output
data Input = Add { pet :: Pet }
           | Remove { pet :: Pet }
             -- level2: Checkout
           | UserLogin { user :: User }
           | AddToBasket { user :: User, pet :: Pet }
           | RemoveFromBasket { user :: User, pet :: Pet }
           | CheckoutBasket { user :: User, payment :: Payment }
           | UserLogout { user :: User }
             -- level3: AddAccessory/RemoveAccessory w/ constraints depending on type of pet
           | ListPets
  deriving (Eq, Show,Generic,ToJSON,FromJSON)

data Output = PetAdded { pet :: Pet }
            | PetRemoved { pet :: Pet }
            | UserLoggedIn { user :: User }
            | AddedToBasket { user :: User, pet :: Pet }
            | RemovedFromBasket { user :: User, pet :: Pet }
            | CheckedOutBasket { user :: User, payment :: Payment }
            | UserLoggedOut { user :: User }
            | Pets { pets :: [ Pet ] }
            | Error { reason :: PetStoreError }
  deriving (Eq, Show,Generic,ToJSON,FromJSON)

-- some errors
data PetStoreError = PetAlreadyAdded
                   | PetDoesNotExist
                   | UserNotLoggedIn
  deriving (Eq, Show,Generic,ToJSON,FromJSON)
