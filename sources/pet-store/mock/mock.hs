{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Options.Applicative
import           PetStore.Mock
import           System.Environment

data Options = Options
    { serverPort :: Int
    }
    deriving (Eq, Show)

optionsParser :: ParserInfo Options
optionsParser =
  info (helper <*> mockOptions)
  ( header "Mock PetStore API"
    <> fullDesc
  )

mockOptions :: Parser Options
mockOptions =
  Options <$> portOption

portOption :: Parser Int
portOption =
  option auto (long "server-port"
               <> short 'p'
               <> value 7890
               <> metavar "INT"
               <> help "port to listen on (default: 7890)"
              )

main :: IO ()
main = do
  Options{..} <- execParser optionsParser
  startMockServer serverPort
