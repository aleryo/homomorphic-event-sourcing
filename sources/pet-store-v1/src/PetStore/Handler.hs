{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
module PetStore.Handler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           PetStore.Log
import           PetStore.Messages
import           PetStore.Store
import           Servant

type PetServer m a =
  (MonadLog m, MonadReader StoreDB m, MonadError ServantErr m, MonadIO m) => m a

listPets :: PetServer m Output
listPets =  ask >>= send ListPets

addPet :: Pet -> PetServer m Output
addPet pet =  ask >>= send (Add pet)

removePet :: Pet -> PetServer m Output
removePet pet =  ask >>= send (Remove pet)

reset :: PetServer m NoContent
reset =  ask >>= resetStore >> pure NoContent
