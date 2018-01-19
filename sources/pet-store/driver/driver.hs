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
reset     :: ClientM NoContent

(listPets :<|> addPet :<|> removePet) :<|> reset = client devPetStoreApi

handleInput :: MonadIO f
            => t -> ClientM a -> ClientEnv -> f (Maybe a, t)
handleInput curSt act = (first eitherToMaybe . (, curSt) <$>) . liftIO . runClientM act

instance Interpreter (ReaderT ClientEnv IO) PetStore PetStoreState Input Output where
  interpret curSt (Add p)    = ask >>= handleInput curSt (addPet p)
  interpret curSt (Remove p) = ask >>= handleInput curSt (removePet p)
  interpret curSt  ListPets  = ask >>= handleInput curSt listPets

  before _ = ask >>= void . liftIO . runClientM reset

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
          env = ClientEnv mgr url

      res <- flip runReaderT env $ testSUT (init :: PetStore) (T trace)

      putStrLn $ "final result " <> show res
      pure res
    assert (isSuccessful b')
