{-# LANGUAGE ViewPatterns #-}
module Acquire.ModelSpec where

import           Acquire.Messages
import           Acquire.Model
import           Acquire.Net             (Result)
import           Control.Monad.Reader
import           IOAutomaton             as A
import           Test.Hspec
import           Test.QuickCheck         hiding (Result)
import           Test.QuickCheck.Monadic

spec :: Spec
spec = describe "Acquire Model" $

  it "validates game engine" $ monadicIO prop_gameEngineRespectsItsModel

prop_gameEngineRespectsItsModel :: PropertyM IO ()
prop_gameEngineRespectsItsModel =
  forAllM (arbitrary :: Gen (Valid GameState AcquireState Message Result))  $ \ (validTransitions -> trace) -> do
  b' <- run $ do
    (t, p) <- startServer
    res <- flip runReaderT ("localhost" :: String, p) $ A.testSUT (A.init :: GameState) (A.T trace)
    stopServer (t,p)
    pure res
  assert (isSuccessful b')
