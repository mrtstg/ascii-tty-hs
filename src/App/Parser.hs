module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runParser :: Parser AppCommand
runParser = pure Run

appParser :: Parser AppOpts
appParser = AppOpts <$>
  strOption (long "frames" <> short 'f' <> metavar "PATH" <> value "./frames/" <> help "Folder with images") <*>
  subparser (
    command "run" (info runParser (progDesc "Start program"))
    ) <*>
  option auto (long "pause" <> short 'p' <> value 1000000 <> help "Sleep N ms after frame change") <*>
  option auto (long "start" <> short 's' <> value 1 <> help "Start at Nth frame") <*>
  switch (long "shuffle" <> help "Shuffle frames after complete slideshow")
