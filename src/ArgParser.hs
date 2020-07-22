{-# LANGUAGE DuplicateRecordFields #-}
module ArgParser where

import Options.Applicative
import Data.Time

data Command
  = Add AddOptionsProps
  | List
  | Remove Int
  deriving Show

data AddOptionsProps = AddOptionsProps
  { title :: String
  , date :: Maybe Day
  } deriving Show

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$>
            (AddOptionsProps <$> strOption (long "title" <> short 't')
            <*> optional (option dayReader (long "date" <> short 'd')))

removeParser :: Parser Command
removeParser = Remove <$> option auto (long "index" <> short 'i')


dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%F" arg of
        Nothing -> Left ("Cannot parse date: " ++ arg)
        Just day -> Right day

commands :: Parser Command
commands =
    subparser (command "add" (info addParser (progDesc "Add event"))) <|>
    subparser (command "list" (info listParser (progDesc "List all events"))) <|>
    subparser (command "remove" (info removeParser (progDesc "Remove event by")))

parseArgs :: IO Command
parseArgs = execParser (info commands idm)