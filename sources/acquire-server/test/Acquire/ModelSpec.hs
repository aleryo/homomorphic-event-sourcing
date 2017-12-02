{-# LANGUAGE ViewPatterns #-}
module Acquire.ModelSpec where

import           Acquire.Messages
import           Acquire.Model
import           Acquire.Net             (Result)
import           Control.Monad.State
import           Data.Monoid             ((<>))
import           IOAutomaton             as A
import           System.IO               (Handle)
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
    putStrLn $ "checking trace " <> show trace
    (t, p) <- startServer
    res <- flip evalStateT ("localhost" :: String, p, Nothing :: Maybe Handle) $ A.testSUT (A.init :: GameState) (A.T trace)
    stopServer (t,p)
    putStrLn $ "final result " <> show res
    pure res
  assert (isSuccessful b')
