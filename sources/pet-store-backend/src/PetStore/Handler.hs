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
removePet pet = withinLog (Remove pet) $ ask >>= send (PetRemoved pet)

reset :: PetServer m NoContent
reset = withinLog ("reset" :: String) $ ask >>= resetStore >> pure NoContent

login            :: User -> PetServer m Output
login user = withinLog (UserLogin user) $ ask >>= send (UserLoggedIn user)

logout           :: User -> PetServer m Output
logout user = withinLog (UserLogout user) $ ask >>= send (UserLoggedOut user)

addToBasket      :: User -> Pet -> PetServer m Output
addToBasket user pet = withinLog (AddToBasket user pet) $ ask >>= send (AddedToBasket user pet)

removeFromBasket :: User -> Pet -> PetServer m Output
removeFromBasket user pet = withinLog (RemoveFromBasket user pet) $ ask >>= send (RemovedFromBasket user pet)

checkout         :: User -> Payment -> PetServer m Output
checkout user payment = withinLog (CheckoutBasket user payment) $ ask >>= send (CheckedOutBasket user payment 0)

listBasket       :: User -> PetServer m Output
listBasket user = withinLog (GetUserBasket user) $ ask >>= const (pure $ UserBasket user [])
