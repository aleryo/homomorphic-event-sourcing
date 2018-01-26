{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module PetStore.Driver where


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
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

listPets         :: ClientM Output
addPet           :: Pet -> ClientM Output
removePet        :: Pet -> ClientM Output
reset            :: ClientM NoContent
login            :: User -> ClientM Output
logout           :: User -> ClientM Output
addToBasket      :: User -> Pet -> ClientM Output
removeFromBasket :: User -> Pet -> ClientM Output
checkout         :: User -> Payment -> ClientM Output
listBasket       :: User -> ClientM Output

(listPets :<|> addPet :<|> removePet:<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout :<|> listBasket) :<|> reset = client devPetStoreApi

handleInput :: MonadIO f
            => t -> ClientM a -> ClientEnv -> f (Maybe a, t)
handleInput curSt act = (first eitherToMaybe . (, curSt) <$>) . liftIO . runClientM act

instance Interpreter (ReaderT ClientEnv IO) PetStore PetStoreState Input Output where
  interpret curSt (Add p)    = ask >>= handleInput curSt (addPet p)
  interpret curSt (Remove p) = ask >>= handleInput curSt (removePet p)
  interpret curSt  ListPets  = ask >>= handleInput curSt listPets

  interpret curSt (UserLogin { user })               = ask >>= handleInput curSt (login user)
  interpret curSt (AddToBasket { user, pet })        = ask >>= handleInput curSt (addToBasket user pet)
  interpret curSt (RemoveFromBasket { user, pet })   = ask >>= handleInput curSt (removeFromBasket user pet)
  interpret curSt (CheckoutBasket { user, payment }) = ask >>= handleInput curSt (checkout user payment)
  interpret curSt (UserLogout { user })              = ask >>= handleInput curSt (logout user)
  interpret curSt (GetUserBasket { user })           = ask >>= handleInput curSt (listBasket user)

  before _ = ask >>= void . liftIO . runClientM reset

  after _  = pure ()

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _)  = Nothing

runTestDriver :: String -> Int -> IO ()
runTestDriver serverHost serverPort = do
  quickCheck $ monadicIO $
    forAllM (arbitrary :: Gen (Valid PetStore PetStoreState Input Output))  $ \ (validTransitions -> trace) -> do
    b' <- run $ do
      mgr <- newManager defaultManagerSettings
      let url = BaseUrl Http serverHost serverPort ""
          env = ClientEnv mgr url

      flip runReaderT env $ testSUT (init :: PetStore) (T trace)

    monitor (counterexample $ "run not successful " <> show b')
    assert (isSuccessful b')
