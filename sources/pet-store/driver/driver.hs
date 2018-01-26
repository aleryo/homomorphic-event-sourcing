{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

import           PetStore.Driver
import           System.Environment

main :: IO ()
main = do
  [serverHost, serverPort] <- getArgs -- to connect to server
  runTestDriver serverHost (read serverPort)
