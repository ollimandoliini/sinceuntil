module Main where

import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad.Reader
import Text.PrettyPrint.Boxes

import ArgParser (parseArgs, Command(..))
import Database (insertEvent, listEvents, Event(..))
import PrettyPrinter

main :: IO ()
main = do
  arguments <- parseArgs
  let reader = executeCommand arguments
  now <- getCurrentTime
  runReaderT reader $ utctDay now

executeCommand :: Command -> ReaderT Day IO ()
executeCommand (Add a) = liftIO $ insertEvent a
executeCommand List     = do
  events <- liftIO listEvents
  table <- createTable events
  liftIO $ printBox table
