module Acquire.Trace(trace) where

import           Control.Monad.Trans
import           Data.Time.Clock
import           Data.Time.Format
import           System.IO           (hPutStrLn, stdout)

trace :: (MonadIO m) => String -> m ()
trace msg = liftIO $ do
  t <- getCurrentTime
  hPutStrLn stdout $ "[" ++ formatTime defaultTimeLocale "%F %T.%q %z" t ++ "] " ++ msg
