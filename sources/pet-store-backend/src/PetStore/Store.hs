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
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           PetStore.Messages
import qualified System.IO               as IO
import           System.IO.Error

data Store = Store { storedPets :: [ Pet ]
                   , baskets    :: Map.Map User [ Pet ]
                   , eventSink  :: IO.Handle
                   }

send :: (MonadIO m) => Output -> StoreDB -> m Output
send event storedb = liftIO $ modifyMVar storedb $ \ store@Store{..} -> do
  hPutStrLn eventSink (decodeUtf8 $ encode event)
  IO.hFlush eventSink
  pure (apply event store, event)

resetStore :: (MonadIO m) => StoreDB -> m ()
resetStore db = liftIO $ modifyMVar_  db $ \ Store{..} -> do
  IO.hSeek eventSink IO.AbsoluteSeek 0
  IO.hSetFileSize eventSink 0
  pure $ Store [] Map.empty eventSink

apply :: Output -> Store -> Store
apply PetAdded { pet } store                = store { storedPets = pet : storedPets store }
apply PetRemoved { pet } store              = store { storedPets = List.delete pet $ storedPets store }
apply UserLoggedIn { user } store           = store { baskets = Map.insert user [] (baskets store) }
apply AddedToBasket { user, pet }store      = store { storedPets = List.delete pet $ storedPets store
                                                    , baskets = Map.adjust (pet:) user $ baskets store }
apply RemovedFromBasket { user, pet } store = store { storedPets = pet : storedPets store
                                                    , baskets = Map.adjust (List.delete pet) user $ baskets store }
apply CheckedOutBasket { user } store       = store { baskets = Map.adjust (const []) user $ baskets store  }
apply UserLoggedOut { user } store          = store { baskets = Map.delete user $ baskets store }
apply UserBasket{} store                    = store
apply Pets{} store                          = store
apply Error{} store                         = store

type StoreDB = MVar Store

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  let store = Store [] Map.empty h
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
