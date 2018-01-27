{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module PetStore.Log where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as IO
import           Data.Time.Clock.System
import           Servant                    (NoContent)

class MonadLog m where
  mlog :: (ToJSON a) => a -> m ()


withinLog :: (MonadLog m, Monad m, ToJSON a, ToJSON b) => a -> m b -> m b
withinLog start act = do
  mlog start
  res <- act
  mlog res
  pure res

instance ToJSON NoContent where
  toJSON _ = Null

instance (MonadIO m) => MonadLog m where
  mlog a = liftIO $ do
    ts <- getSystemTime
    IO.putStrLn $ encode $ object [ "timestamp" .= show ts
                                  , "message" .= a
                                  ]
