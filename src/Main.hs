module Main where

import ArgParser (parseArgs, Command(..))
import Database (insertEvent, listEvents)

executeCommand :: Command -> IO ()
executeCommand (Add a) = insertEvent a
executeCommand List    = print =<< listEvents

main :: IO ()
main = do
  arguments <- parseArgs
  executeCommand arguments

