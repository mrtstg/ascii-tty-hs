module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  , AppEnv(..)
  ) where

import qualified Vision.Image as I

data AppEnv = AppEnv
  { supportsANSI :: !Bool
  , images       :: ![I.RGB]
  }

data AppOpts = AppOpts
  { framesPath :: !FilePath
  , appCommand :: !AppCommand
  } deriving Show

data AppCommand = Run deriving (Enum, Eq, Show)
