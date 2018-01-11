{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Arrow           (first)
import           Control.Monad.Reader
import           Data.Monoid             ((<>))
import           IOAutomaton
import           Network.HTTP.Client
import           PetStore.Api
import           PetStore.Messages
import           PetStore.Model
import           Prelude                 hiding (init)
import           Servant
import           Servant.Client
import           System.Environment
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

listPets  :: ClientM Output
addPet    :: Pet -> ClientM Output
removePet :: Pet -> ClientM Output
listPets :<|> addPet :<|> removePet = client petStoreApi

instance Interpreter (ReaderT ClientEnv IO) PetStore PetStoreState Input Output where
  interpret curSt (Add p)    = ask >>= (first eitherToMaybe . (,curSt) <$>) . liftIO . runClientM (addPet p)
  interpret curSt (Remove p) = ask >>= (first eitherToMaybe . (,curSt) <$>) . liftIO . runClientM (removePet p)
  interpret curSt  ListPets  = ask >>= (first eitherToMaybe . (,curSt) <$>) . liftIO . runClientM listPets

  before _ = pure ()
  after _  = pure ()

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _)  = Nothing

main :: IO ()
main = do
  [serverHost, serverPort] <- getArgs -- to connect to server
  quickCheck $ monadicIO $
    forAllM (arbitrary :: Gen (Valid PetStore PetStoreState Input Output))  $ \ (validTransitions -> trace) -> do
    b' <- run $ do
      putStrLn $ "checking trace " <> show trace <> " against " <> show (serverHost, serverPort)
      mgr <- newManager defaultManagerSettings
      let url = BaseUrl Http serverHost (read serverPort) ""
      res <- flip runReaderT (ClientEnv mgr url) $ testSUT (init :: PetStore) (T trace)
      putStrLn $ "final result " <> show res
      pure res
    assert (isSuccessful b')
