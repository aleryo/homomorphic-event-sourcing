{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
module PetStoreSpec where

import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Monoid            ((<>))
import           IOAutomaton
import           PetStore.Messages
import           PetStore.Model
import           Prelude                hiding (init)
import           Test.Hspec
import           Test.QuickCheck

newtype MockPetStore m a = MockPetStore { runMock :: StateT [ Input ] m a }
  deriving (Functor, Applicative, Monad, MonadState [Input])

instance (Monad m) => Interactive (MockPetStore m) Input Output where
  request = do
    reqs <- get
    case reqs of
      (req:reqs') -> put reqs' >> return (Just req)
      []          -> return Nothing

  reply _ = pure ()

newtype ValidPetStore = ValidPetStore (Valid PetStore PetStoreState Input Output)
  deriving (Eq, Show)

instance Arbitrary ValidPetStore where
  arbitrary = ValidPetStore <$> arbitrary

prop_mockPetStoreClientRunsAgainstMockRunner :: Property
prop_mockPetStoreClientRunsAgainstMockRunner =
  verbose $
  forAll (arbitrary :: Gen ValidPetStore) $ \ (ValidPetStore (validTransitions -> trs)) ->
    let res = runIdentity . flip evalStateT (fmap input trs) . runMock $ mockModel (init :: PetStore)
        msg = "result :" <> show res
    in  counterexample msg $ isSuccessful res

spec :: Spec
spec =
  describe "Check Pet Store Model" $
  it "is consistent"$ property prop_mockPetStoreClientRunsAgainstMockRunner
