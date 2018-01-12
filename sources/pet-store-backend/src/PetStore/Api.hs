{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module PetStore.Api where

import           PetStore.Messages
import           Servant

type PetStoreApi = "pets"   :> Get '[JSON] Event
                   :<|> "pets"   :> ReqBody '[JSON] Pet :> Post '[JSON] Event
                   :<|> "pets"   :> ReqBody '[JSON] Pet :> Delete '[JSON] Event

type DevPetStoreApi = PetStoreApi
                      :<|> "_reset" :> Delete '[JSON] NoContent

petStoreApi :: Proxy PetStoreApi
petStoreApi = Proxy


devPetStoreApi :: Proxy DevPetStoreApi
devPetStoreApi = Proxy
