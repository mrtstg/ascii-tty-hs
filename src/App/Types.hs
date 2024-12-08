module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  , AppEnv(..)
  ) where

import qualified Vision.Image as I

data AppEnv = AppEnv
  { supportsANSI   :: !Bool
  , images         :: ![I.RGB]
  , framePauseTime :: !Int
  }

data AppOpts = AppOpts
  { framesPath :: !FilePath
  , appCommand :: !AppCommand
  , pauseTime  :: !Int
  } deriving Show

data AppCommand = Run deriving (Enum, Eq, Show)
