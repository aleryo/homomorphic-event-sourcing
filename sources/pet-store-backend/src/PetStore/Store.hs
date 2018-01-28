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
import           Data.Map                (member)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           PetStore.Log
import           PetStore.Messages
import qualified System.IO               as IO
import           System.IO.Error

data Store = Store { storedPets :: [ Pet ]
                   , baskets    :: Map.Map User [ Pet ]
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
  pure $ Store [] Map.empty eventSink

act :: Input -> Store -> Output
act Add{pet}  store@Store{storedPets}
  | pet `notElem` storedPets = PetAdded pet
  | otherwise                = Error PetAlreadyAdded

act Remove{pet}  store@Store{storedPets}
  | pet `notElem` storedPets = Error PetDoesNotExist
  | otherwise                = PetRemoved pet

-- Actually a Query not a Command
act ListPets          s@Store{storedPets}
  = Pets storedPets

act UserLogin { user }               store@Store{ baskets}
  = UserLoggedIn user

act AddToBasket { user, pet }        store@Store{storedPets, baskets}
  | not (user `member` baskets) = Error UserNotLoggedIn
  | pet `notElem` storedPets    = Error PetDoesNotExist
  | otherwise                   = AddedToBasket user pet

act RemoveFromBasket { user, pet}    store@Store{storedPets, baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | fmap (pet `notElem`) userBasket
    == Just True                     = Error PetNotInBasket
  | otherwise                        = RemovedFromBasket user pet

  where
    userBasket = Map.lookup user baskets

act GetUserBasket { user } store@Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | otherwise                    = UserBasket user userBasket
  where
    userBasket     = fromJust $ Map.lookup user baskets

act CheckoutBasket { user, payment } store@Store{baskets}
  | not (user `member` baskets)      = Error UserNotLoggedIn
  | checkCardNumber payment          = CheckedOutBasket user payment basketAmount
  | otherwise                        = Error InvalidPayment
  where
    userBasket     = fromJust $ Map.lookup user baskets
    basketAmount   = foldr (+) 0 $ fmap petPrice userBasket

act UserLogout { user }              store@Store{storedPets, baskets}
  | not (user `member` baskets)      = Error UserNotLoggedIn
  | otherwise                        = UserLoggedOut user


apply :: Output -> Store -> Store
apply PetAdded { pet } store                = store { storedPets = pet : storedPets store }
apply PetRemoved { pet } store              = store { storedPets = List.delete pet $ storedPets store }
apply UserLoggedIn { user} store
  | not (user `member` baskets store)       = store { baskets = Map.insert user [] (baskets store) }
  | otherwise                               = store
apply AddedToBasket { user, pet }store      = store { storedPets = List.delete pet $ storedPets store
                                                    , baskets = Map.adjust (pet:) user $ baskets store }
apply RemovedFromBasket { user, pet } store = store { storedPets = pet : storedPets store
                                                    , baskets = Map.adjust (List.delete pet) user $ baskets store }
apply CheckedOutBasket { user } store       = store { baskets = Map.adjust (const []) user $ baskets store }
apply UserLoggedOut { user } store          = store { storedPets = putbackUserBasket (storedPets store)
                                                    , baskets = Map.delete user $ baskets store }
  where
    putbackUserBasket pets =
      case Map.lookup user (baskets store) of
        Nothing -> pets
        Just ps -> pets <> ps

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
