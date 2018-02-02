{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

--import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
--import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid                               ((<>))
--import           Data.Text
--import           Data.Text.Encoding       (encodeUtf8)
import           Data.Default
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           PetStore.Api
import           PetStore.Handler
import           PetStore.Store
import           PetStore.Swagger
import           Servant


startServer :: ServerMode -> Int -> IO ()
startServer devMode port = do
  putStrLn $ "Starting PetStore Server: " <> show port
  store <- makeStore
  logger <- doLog devMode
  void $ run port $ logger $ server store devMode
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      runServer store = NT $ Handler . flip runReaderT store

      server store Prod = serve petStoreApi $ enter (runServer store) prodHandler
      server store Dev  = serve devPetStoreApi $ enter (runServer store) devHandler

      prodHandler = listPets :<|> addPet :<|> removePet
      devHandler  = prodHandler :<|> reset :<|> pure petStoreSwagger
