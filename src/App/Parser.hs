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
    )
