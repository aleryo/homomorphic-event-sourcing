{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

--import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
--import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid                          ((<>))
--import           Data.Text
--import           Data.Text.Encoding       (encodeUtf8)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger
import           PetStore.Api
import           PetStore.Handler
import           PetStore.Store
import           Servant

startServer :: ServerMode -> Int -> IO ()
startServer devMode port = do
  putStrLn $ "Starting PetStore Server: " <> show port
  store <- makeStore
  void $ run port $ doLog devMode $ server store devMode
    where
      doLog Dev  = logStdoutDev
      doLog Prod = logStdout

      runServer store = NT $ Handler . flip runReaderT store

      server store Prod = serve petStoreApi $ enter (runServer store) prodHandler
      server store Dev  = serve devPetStoreApi $ enter (runServer store) devHandler

      prodHandler = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout :<|> listBasket
      devHandler  = prodHandler :<|> reset
