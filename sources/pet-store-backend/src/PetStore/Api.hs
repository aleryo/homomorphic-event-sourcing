{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module PetStore.Api where

import           Data.Text         (pack, unpack)
import           PetStore.Messages
import           Servant

type PetStoreApi = "pets"  :> Get '[JSON] Output
              :<|> "pets"  :> ReqBody '[JSON] Pet :> Post '[JSON] Output
              :<|> "pets"  :> ReqBody '[JSON] Pet :> Delete '[JSON] Output
              :<|> "users" :> Capture "user" User :> Put '[JSON] Output
              :<|> "users" :> Capture "user" User :> Delete '[JSON] Output
              :<|> "users" :> Capture "user" User :> ReqBody '[JSON] Pet :> Put '[JSON] Output
              :<|> "users" :> Capture "user" User :> ReqBody '[JSON] Pet :> Delete '[JSON] Output
              :<|> "users" :> Capture "user" User :> ReqBody '[JSON] Payment :> Post '[JSON] Output
              :<|> "users" :> Capture "user" User :> Get '[JSON] Output

type DevPetStoreApi = PetStoreApi
                      :<|> "_reset" :> Delete '[JSON] NoContent

data ServerMode = Prod | Dev
  deriving (Eq, Show, Read)

petStoreApi :: Proxy PetStoreApi
petStoreApi = Proxy

devPetStoreApi :: Proxy DevPetStoreApi
devPetStoreApi = Proxy

instance FromHttpApiData User where
  parseQueryParam = Right . User . unpack

instance ToHttpApiData User where
  toQueryParam (User u) = pack u
