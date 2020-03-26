{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Options.Applicative
import           PetStore.Api
import           PetStore.Server
import           System.Environment

data Options = Options
    { serverPort :: Int
    , runMode    :: ServerMode
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
  Options <$> portOption <*> modeOption

portOption :: Parser Int
portOption =
  option auto (long "server-port"
               <> short 'p'
               <> value 7890
               <> metavar "INT"
               <> help "port to listen on (default: 7890)"
              )

modeOption :: Parser ServerMode
modeOption =
  flag Prod Dev  (long "development"
                  <> short 'd'
                  <> help "run server in development mode"
                 )


main :: IO ()
main = do
  Options{..} <- execParser optionsParser
  startServer runMode serverPort
