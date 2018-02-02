{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module PetStore.Api where

import           Data.Swagger
import           PetStore.Messages
import           Servant

data ServerMode = Prod | Dev
  deriving (Eq, Show, Read)

type PetStoreApi = "pets"   :> Get '[JSON] Output
                   :<|> "pets"   :> ReqBody '[JSON] Pet :> Post '[JSON] Output
                   :<|> "pets"   :> ReqBody '[JSON] Pet :> Delete '[JSON] Output

type DevPetStoreApi = PetStoreApi
                      :<|> "_reset" :> Delete '[JSON] NoContent
                      :<|> "swagger.json" :> Get '[JSON] Swagger

petStoreApi :: Proxy PetStoreApi
petStoreApi = Proxy


devPetStoreApi :: Proxy DevPetStoreApi
devPetStoreApi = Proxy
