{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           PetStore.Mock
import           System.Environment

main :: IO ()
main = do
  [port] <- getArgs
  startMockServer (read port)
