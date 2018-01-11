{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module PetStore.Api where

import           PetStore.Messages
import           Servant

type PetStoreApi = "pets" :> Get '[JSON] Output
                   :<|> "pets" :> ReqBody '[JSON] Pet :> Post '[JSON] Output
                   :<|> "pets" :> ReqBody '[JSON] Pet :> Delete '[JSON] Output

petStoreApi :: Proxy PetStoreApi
petStoreApi = Proxy
