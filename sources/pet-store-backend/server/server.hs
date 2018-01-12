{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

--import           Control.Concurrent.MVar
import           Control.Monad.Except
--import           Control.Monad.Reader
--import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid              ((<>))
--import           Data.Text
--import           Data.Text.Encoding       (encodeUtf8)
import           Network.Wai.Handler.Warp (run)
import           PetStore.Api
--import           PetStore.Messages
import           Servant
import           System.Environment

main :: IO ()
main = do
  [devMode, port] <- getArgs
  putStrLn $ "Starting PetStore Server: " <> port
  void $ run (read port) $ server devMode
    where
      server "prod" = serve petStoreApi prodHandler
      server "dev"  = serve devPetStoreApi devHandler
      server other  = error $ "don't know how to handle environment '" <> other <>  "'"

      prodHandler = listPets :<|> addPet :<|> removePet
      devHandler = (listPets :<|> addPet :<|> removePet) :<|> reset

      listPets = undefined
      addPet = undefined
      removePet = undefined
      reset = undefined
