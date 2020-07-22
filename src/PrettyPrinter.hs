module PrettyPrinter where

import Text.PrettyPrint.Boxes
import Control.Monad.Reader
import Data.Time.Calendar
import ArgParser (Command(..))
import Database (insertEvent, listEvents, Event(..))

createTable :: [Event] -> ReaderT Day IO Box
createTable events = do
  today <- ask
  let columns = hsep 5 left [
                mkCol "Index" (show . index)
                , mkCol "Title" title
                , mkCol "Date" (show . date)
                , mkCol "Since/Until" (show . diffDays today . date)
                ]

  return columns
  where
    mkCol header accessor = text header //  vcat left (map (text . accessor) events)
    dayDiffCol today = text "Since/Until" // vcat left (map (text . show . diffDays today . date) events)