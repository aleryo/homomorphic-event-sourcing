{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           PetStore.Server
import           System.Environment

main :: IO ()
main = do
  [devMode, port] <- getArgs
  startServer (read devMode) (read port)
