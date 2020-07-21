{-# LANGUAGE OverloadedStrings #-}
module Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import System.Directory (doesFileExist)

import qualified ArgParser as A


data Event = Event 
  { index :: Int
  , title :: String
  , date :: Day
}
    
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field


dbConnection :: IO Connection
dbConnection = do 
  conn <- open "events.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS events (id INTEGER PRIMARY KEY, title TEXT, date TEXT)"
  return conn


insertEvent :: A.AddOptionsProps -> IO ()
insertEvent props =
  dbConnection >>= \conn -> execute conn "INSERT INTO events (title, date) VALUES (?, ?)" params
  where
    params = (A.title props, A.date props)

listEvents :: IO [Event]
listEvents =
  dbConnection >>= \conn -> query_ conn "SELECT * FROM events" :: IO [Event]