module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  , AppEnv(..)
  ) where

import           System.Random (StdGen)

import qualified Vision.Image  as I

data AppEnv = AppEnv
  { supportsANSI   :: !Bool
  , images         :: ![I.RGB]
  , frameAmount    :: !Int
  , framePauseTime :: !Int
  , shuffleFrames  :: !Bool
  }

data AppOpts = AppOpts
  { framesPath :: !FilePath
  , appCommand :: !AppCommand
  , pauseTime  :: !Int
  , startAt    :: !Int
  , shuffle    :: !Bool
  } deriving Show

data AppCommand = Run deriving (Enum, Eq, Show)
