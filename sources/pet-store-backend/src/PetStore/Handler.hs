{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
module PetStore.Handler where

import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           PetStore.Log
import           PetStore.Messages
import           PetStore.Store
import           Servant

type PetServer m a =
  (MonadLog m, MonadReader StoreDB m, MonadError ServantErr m, MonadIO m) => m a

listPets :: PetServer m Output
listPets = withinLog ListPets $ ask >>= liftIO . readMVar >>= pure . Pets . storedPets


addPet :: Pet -> PetServer m Output
addPet pet = withinLog (Add pet) $ ask >>= send (PetAdded pet)


removePet :: Pet -> PetServer m Output
removePet = undefined

reset :: PetServer m NoContent
reset = withinLog ("reset" :: String) $ ask >>= resetStore >> pure NoContent

login            :: User -> PetServer m Output
login = undefined

logout           :: User -> PetServer m Output
logout = undefined

addToBasket      :: User -> Pet -> PetServer m Output
addToBasket = undefined

removeFromBasket :: User -> Pet -> PetServer m Output
removeFromBasket = undefined

checkout         :: User -> Payment -> PetServer m Output
checkout = undefined

listBasket       :: User -> PetServer m Output
listBasket = undefined
