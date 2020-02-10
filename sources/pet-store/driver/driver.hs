{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

import           Options.Applicative
import           PetStore.Driver
import           System.Environment

data Options = Options
    { serverPort :: Int
    , serverHost :: String
    }
    deriving (Eq, Show)

optionsParser :: ParserInfo Options
optionsParser =
  info (helper <*> mockOptions)
  ( header "PetStore Driver API"
    <> fullDesc
  )

mockOptions :: Parser Options
mockOptions =
  Options <$> portOption <*> hostOption

portOption :: Parser Int
portOption =
  option auto (long "server-port"
               <> short 'p'
               <> value 7890
               <> metavar "INT"
               <> help "port to connect to (default: 7890)"
              )

hostOption :: Parser String
hostOption =
  strOption (long "server-host"
               <> short 'h'
               <> value "localhost"
               <> metavar "STRING"
               <> help "host to connect to (default: 'localhost')"
              )

main :: IO ()
main = do
  Options{..} <- execParser optionsParser
  runTestDriver serverHost serverPort
