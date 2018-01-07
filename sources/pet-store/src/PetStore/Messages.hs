module PetStore.Messages where

data Input = CheckIn { petName :: String }
           | CheckOut { petName :: String }
           | ListPets

data Output = Pets { pets :: [ String ] }
            | Error { reason :: String }
