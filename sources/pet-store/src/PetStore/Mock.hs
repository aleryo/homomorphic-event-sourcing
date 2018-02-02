{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Mock where


import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Monoid                          ((<>))
import           Data.Text
import           Data.Text.Encoding                   (encodeUtf8)
import qualified IOAutomaton                          as A
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger
import           PetStore.Api
import           PetStore.Messages
import           PetStore.Model
import           Servant

startMockServer :: Int -> IO ()
startMockServer port = do
  store <- newMVar (PetStore [] [])
  putStrLn $ "starting mock HTTP PetStore on port " <> show port
  void $ run port $ logStdoutDev $ (serve devPetStoreApi $ enter (runServer store)  handler)
    where
      runServer store = NT $ Handler . flip runReaderT store

      handler = (listPets :<|> addPet :<|> removePet
                :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout :<|> listBasket)
        :<|> reset :<|> undefined

      addPet    pet = action (Add pet)
      removePet pet = action (Remove pet)
      listPets      = action ListPets

      login  = action . UserLogin
      logout = action . UserLogout
      addToBasket user pet = action (AddToBasket user pet)
      removeFromBasket user pet = action (RemoveFromBasket user pet)
      checkout user payment = action (CheckoutBasket user payment)
      listBasket user = action (GetUserBasket user)

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
