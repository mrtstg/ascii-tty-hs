module Main (main) where

import           App.Commands
import           App.Parser
import           App.Types
import           Options.Applicative

main :: IO ()
main = do
  opts <- execParser (info (appParser <**> helper) fullDesc)
  runCommand opts
