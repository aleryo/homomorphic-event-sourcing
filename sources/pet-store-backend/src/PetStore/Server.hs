{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

--import           Control.Concurrent.MVar
import           Control.Monad.Except
--import           Control.Monad.Reader
--import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid              ((<>))
--import           Data.Text
--import           Data.Text.Encoding       (encodeUtf8)
import           Network.Wai.Handler.Warp (run)
import           PetStore.Api
import           PetStore.Handler
--import           PetStore.Messages
import           Servant

startServer :: ServerMode -> Int -> IO ()
startServer devMode port = do
  putStrLn $ "Starting PetStore Server: " <> show port
  void $ run port $ server devMode
    where
      server Prod = serve petStoreApi prodHandler
      server Dev  = serve devPetStoreApi devHandler

      prodHandler = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout :<|> listBasket
      devHandler  = prodHandler :<|> reset
