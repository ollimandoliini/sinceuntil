module PrettyPrinter where

import Text.PrettyPrint.Boxes
import Control.Monad.Reader
import Data.Time.Calendar


import ArgParser (Command(..))
import Database (insertEvent, listEvents, Event(..))



createTable :: [Event] -> ReaderT Day IO Box
createTable events = do
  today <- ask
  return $ hsep 5 left [indexCol, titleCol, dateCol, dayDiffCol today]
  where
    indexCol = text "Index" //  vcat left (map (text . show .  index) events)
    titleCol = text "Title" // vcat left (map (text . title) events)
    dateCol  = text "Date" // vcat left (map (text . show . date) events)
    dayDiffCol today = text "Since/Until" // vcat left (map (text . show . diffDays today . date) events)