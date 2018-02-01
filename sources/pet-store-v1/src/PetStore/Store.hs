{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | A backend store based on event sourcing
--
--  * Events affecting store are persisted as a stream of events into a file
--  * State is cached in memory
--
module PetStore.Store where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Trans     (MonadIO (..))
import           Data.Aeson              (eitherDecode, encode)
import qualified Data.List               as List
import           Data.Monoid
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           PetStore.Log
import           PetStore.Messages
import qualified System.IO               as IO
import           System.IO.Error

data Store = Store { storedPets :: [ Pet ]
                   , eventSink  :: IO.Handle
                   }

send :: (MonadIO m) => Input -> StoreDB -> m Output
send input storedb = withinLog input $ liftIO $ modifyMVar storedb $ \ store@Store{..} -> do
  let event = act input store
  hPutStrLn eventSink (decodeUtf8 $ encode event)
  IO.hFlush eventSink
  pure (apply event store, event)

resetStore :: (MonadIO m) => StoreDB -> m ()
resetStore db = liftIO $ modifyMVar_  db $ \ Store{..} -> do
  IO.hSeek eventSink IO.AbsoluteSeek 0
  IO.hSetFileSize eventSink 0
  pure $ Store [] eventSink

act :: Input -> Store -> Output
act Add{pet}  Store{storedPets}
  | pet `notElem` storedPets = PetAdded pet
  | otherwise                = Error PetAlreadyAdded

act Remove{pet}  Store{storedPets}
  | pet `notElem` storedPets = Error PetDoesNotExist
  | otherwise                = PetRemoved pet

-- Actually a Query not a Command
act ListPets     Store{storedPets}
  = Pets storedPets


apply :: Output -> Store -> Store
apply PetAdded { pet } store                = store { storedPets = pet : storedPets store }
apply PetRemoved { pet } store              = store { storedPets = List.delete pet $ storedPets store }
apply Pets{} store                          = store
apply Error{} store                         = store

type StoreDB = MVar Store

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  let store = Store [] h
  catchup store >>= newMVar

catchup :: Store -> IO Store
catchup store =
  (readAndApplyOneEvent store >>= catchup)
  `catch` \ (e :: IOException) -> if isEOFError e
                                  then pure store
                                  else throwIO e

readAndApplyOneEvent :: Store -> IO Store
readAndApplyOneEvent store@Store{eventSink} = do
  serializedEvent <- hGetLine eventSink
  either
    (throwIO . userError . (\ e -> "failed to decode event " <> show serializedEvent <> ": " <> e))
    (pure . flip apply store)
    $ eitherDecode (encodeUtf8 serializedEvent)
