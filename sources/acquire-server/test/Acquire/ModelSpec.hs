{-# LANGUAGE ViewPatterns #-}
module Acquire.ModelSpec where

import           Acquire.Model
import           Acquire.Net             (Result, randomGameId)
import           Control.Monad.State
import           Data.Monoid             ((<>))
import           IOAutomaton             as A
import           System.Directory        (doesFileExist, removeFile)
import           System.IO               (Handle)
import           Test.Hspec
import           Test.QuickCheck         hiding (Result)
import           Test.QuickCheck.Monadic

spec :: Spec
spec = describe "Acquire Model" $

  it "validates game engine" $ monadicIO prop_gameEngineRespectsItsModel

prop_gameEngineRespectsItsModel :: PropertyM IO ()
prop_gameEngineRespectsItsModel =
  forAllM (arbitrary :: Gen (Valid GameState AcquireState Input Result))  $ \ (validTransitions -> trace) -> do
  b' <- run $ do
    removeStateFile
    putStrLn $ "checking trace " <> show trace
    (t, p) <- startServer
    res <- flip evalStateT ("localhost" :: String, p, Nothing :: Maybe Handle) $ A.testSUT (A.init :: GameState) (A.T trace)
    stopServer (t,p)
    putStrLn $ "final result " <> show res
    pure res
  assert (isSuccessful b')

removeStateFile :: IO ()
removeStateFile = do
  let nextId = randomGameId seed
      file = ".acquire." <> nextId <> ".bak"
  exist <- doesFileExist file
  if exist
    then removeFile file
    else pure ()
