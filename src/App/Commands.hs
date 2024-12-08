{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module App.Commands (runCommand) where

import           App.Types
import           Codec.Picture
import           Control.Concurrent           (threadDelay)
import           Data.List                    (sortOn)
import           Data.Maybe
import qualified Data.Vector.Storable         as V
import           System.Console.ANSI
import           System.Console.Terminal.Size
import           System.Directory
import           System.FilePath
import           System.IO                    (stdout)
import           Text.Read
import qualified Vision.Image                 as I
import           Vision.Image                 (RGBPixel)
import           Vision.Image.JuicyPixels
import           Vision.Primitive.Shape

getTerminalSize' :: (Int, Int) -> IO (Int, Int)
getTerminalSize' imageSize = do
  terminalSize <- size
  case terminalSize of
    Nothing                -> return imageSize
    (Just (Window { .. })) -> return (height, width)

processFrame :: [I.Manifest RGBPixel] -> [I.Manifest RGBPixel] -> IO ()
processFrame allFrames (currentFrame:acc) = do
  terminalSize <- getTerminalSize' (0, 0)
  let (Z :. h :. w) = I.manifestSize currentFrame
  print (h, w)
  print terminalSize
  _ <- threadDelay 1000000
  processFrame allFrames acc
processFrame [] _ = error "No frames provided!"
processFrame allFrames [] = processFrame allFrames allFrames

runRunCommand :: FilePath -> IO ()
runRunCommand framesPath = let
  f :: DynamicImage -> Image PixelRGB8
  f (ImageRGB8 i) = i
  f otherImage    = convertRGB8 otherImage
  in do
  supportsANSI <- hNowSupportsANSI stdout
  frameFiles <- listDirectory framesPath
  -- TODO: not only jpg support
  let filteredNames = sortOn ((\x -> Text.Read.read x :: Int) . dropExtensions) $ filter (isJust . (\x -> readMaybe x :: Maybe Int) . dropExtensions) frameFiles
  imagesLoad <- mapM (readImage . (\x -> framesPath `combine` x)) filteredNames
  case sequence imagesLoad of
    (Left e) -> putStrLn $ "Image load error: " <> show e
    (Right images) -> do
      let convertedImages = map (toFridayRGB . f) images
      putStrLn "Loaded images!"
      _ <- clearScreen
      processFrame convertedImages convertedImages

runCommand :: AppOpts -> IO ()
runCommand (AppOpts { appCommand = Run,.. }) = runRunCommand framesPath
