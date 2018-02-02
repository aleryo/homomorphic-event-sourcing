{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-| Swagger-based documentation for `booking` API -}
module PetStore.Swagger(petStoreSwagger) where

import           Control.Lens
import           Data.Aeson
import           Data.Maybe
import           Data.Swagger
import           GHC.Generics
import           PetStore.Api
import           PetStore.Messages
import           Servant
import           Servant.Swagger

instance ToSchema Output
instance ToSchema PetType
instance ToSchema PetStoreError

instance ToSchema Pet where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
                             & mapped.schema.description ?~ "A Pet for sale in the Store"
                             & mapped.schema.example ?~
                             toJSON (Pet "Fifi" Dog)


petStoreSwagger :: Swagger
petStoreSwagger = toSwagger petStoreApi
  & info.title   .~ "Pet Store API"
  & info.version .~ "1.0"
  & info.description ?~ "An API for managing a Pet Store"
  & info.license ?~ ("All Rights Reserved")
