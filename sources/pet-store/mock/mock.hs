{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid              ((<>))
import           Data.Text
import           Data.Text.Encoding       (encodeUtf8)
import qualified IOAutomaton              as A
import           Network.Wai.Handler.Warp (run)
import           PetStore.Api
import           PetStore.Messages
import           PetStore.Model
import           Servant
import           System.Environment

main :: IO ()
main = do
  [port] <- getArgs
  store <- newMVar (PetStore [] [])
  putStrLn $ "starting mock HTTP PetStore on port " <> port
  void $ run (read port) (serve devPetStoreApi $ enter (runServer store) handler)
    where
      runServer store = NT $ Handler . flip runReaderT store

      handler = (listPets :<|> addPet :<|> removePet) :<|> reset

      addPet    pet = action (Add pet)
      removePet pet = action (Remove pet)
      listPets      = ask >>= (Pets . storedPets <$>) . liftIO . readMVar
      reset         = do
        st <- ask
        liftIO $ modifyMVar st (const $ pure (A.init, NoContent))

      action act =  ask >>= \ st -> do
        pets <- liftIO $ takeMVar st

        let (res, pets') = A.action act pets

        liftIO $ putMVar st pets'
        case res of
          Just output -> return output
          Nothing     -> throwError $ err400 { errBody = LBS.fromStrict $ encodeUtf8 $ pack $ "failed to " <> show act }
