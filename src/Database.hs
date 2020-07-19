{-# LANGUAGE OverloadedStrings #-}
module Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import qualified ArgParser as A

data Event = Event 
  { index :: Int
  , title :: String
  , date :: String
} deriving Show 

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

dbConnection :: IO Connection
dbConnection = open "events.db"

insertEvent :: A.AddOptionsProps -> IO ()
insertEvent add =
  dbConnection >>= \conn -> execute conn "INSERT INTO events (title, date) VALUES (?, ?)" params
  where
    params = (A.title add, A.date add)

listEvents :: IO [Event]
listEvents =
  dbConnection >>= \conn -> query_ conn "SELECT * FROM events" :: IO [Event]