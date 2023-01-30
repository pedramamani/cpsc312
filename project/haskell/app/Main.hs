module Main where

import qualified Data.Monoid as Monoid
import qualified Interface
import qualified Base
import Options.Applicative

data Command
  = Load FilePath
  | Reload
  | Start String
  | Stop
  | Cancel
  | Status
  | Reset
  | View String String String

parseActivity :: Parser String
parseActivity = argument str (metavar "ACTIVITY_NAME")

parseFilePath :: Parser FilePath
parseFilePath = argument str (metavar "FILE_PATH")

parseDate :: Parser FilePath
parseDate = argument str (metavar "DATE")

loadParser :: ParserInfo Command
loadParser =
  info
    (Load <$> parseFilePath)
    (progDesc "Load activity tree from a text file")

reloadParser :: ParserInfo Command
reloadParser =
  info
    (pure Reload)
    (progDesc "Reload the current activity tree")

startParser :: ParserInfo Command
startParser =
  info
    (Start <$> parseActivity)
    (progDesc "Start recording an activity")

stopParser :: ParserInfo Command
stopParser =
  info
    (pure Stop)
    (progDesc "Stop recording the current activity")

cancelParser :: ParserInfo Command
cancelParser =
  info
    (pure Cancel)
    (progDesc "Cancel the current activity recording")

statusParser :: ParserInfo Command
statusParser =
  info
    (pure Status)
    (progDesc "Get current activity status")

resetParser :: ParserInfo Command
resetParser =
  info
    (pure Reset)
    (progDesc "Erase activity tree and all recordings")

viewParser :: ParserInfo Command
viewParser =
  info
    (View <$> parseActivity <*> parseDate <*> parseDate)
    (progDesc "View a summary of all sub-activities in given date range")

commandParser :: ParserInfo Command
commandParser = info (commands <**> helper) $ progDesc "Liva - Keep track of your life"
  where
    commands =
      subparser $
        Monoid.mconcat
          [ command "load" loadParser,
            command "reload" reloadParser,
            command "start" startParser,
            command "stop" stopParser,
            command "cancel" cancelParser,
            command "status" statusParser,
            command "reset" resetParser,
            command "view" viewParser
          ]

main :: IO ()
main = do
  c <- execParser commandParser
  case c of
    Load f -> Interface.load f
    Reload -> Interface.reload
    Start s -> Interface.start (Base.parse s)
    Stop -> Interface.stop
    Cancel -> Interface.cancel
    Status -> Interface.status
    Reset -> Interface.reset
    View a sd ed -> Interface.view (Base.parse a) (Base.parse sd) (Base.parse ed)
