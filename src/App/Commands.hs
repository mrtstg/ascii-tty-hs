{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module App.Commands (runCommand) where

import           App.Types
import           Codec.Picture
import           Control.Concurrent           (threadDelay)
import           Control.Monad                (when)
import           Data.List                    (intercalate, sortOn)
import           Data.List.Split
import           Data.Maybe
import qualified Data.Vector                  as VV
import           Data.Vector.Generic          (convert)
import qualified Data.Vector.Storable         as V
import           Data.Word
import           System.Console.ANSI
import           System.Console.ANSI.Types
import           System.Console.Terminal.Size
import           System.Directory
import           System.FilePath
import           System.IO                    (stdout)
import           System.Random                (newStdGen)
import           System.Random.Shuffle
import           Text.Read
import qualified Vision.Image                 as I
import           Vision.Image                 (RGBPixel)
import           Vision.Image.JuicyPixels
import qualified Vision.Image.Transform       as T
import           Vision.Primitive.Shape

asciiChars = reverse ['@', '#', '$', '%', '?', '*', '+', ';', ':', ',', '.']

getTerminalSize' :: (Int, Int) -> IO (Int, Int)
getTerminalSize' imageSize = do
  terminalSize <- size
  case terminalSize of
    Nothing                -> return imageSize
    (Just (Window { .. })) -> return (height, width)

processFrame :: AppEnv -> [I.Manifest RGBPixel] -> IO ()
processFrame env@(AppEnv { .. }) (currentFrame:acc) = let
  f :: RGBPixel -> (Char, Word8)
  f (I.RGBPixel r g b) = (asciiChars !! (avg `div` 25), xterm6LevelRGB (fromIntegral r `div` 43) (fromIntegral g `div` 43) (fromIntegral b `div` 43)) where
    avg :: Int
    avg = fromIntegral (r + g + b)

  f' :: Bool -> [(Char, Word8)] -> IO ()
  f' _ [] = putStr "\n"
  f' useColor ((sym, c):lst) = do
    when useColor $ setSGR [SetPaletteColor Foreground c]
    putStr [sym]
    f' useColor lst

  f'' :: Int -> Bool -> [(Char, Word8)] -> IO ()
  f'' spaceAmount ansi row = do
    _ <- putStr $ replicate spaceAmount ' '
    f' ansi row
  in do
  let (Z :. h :. w) = I.manifestSize currentFrame

  (tH, tW) <- getTerminalSize' (h, w)

  let ratioH = fromIntegral (min 101 tH) / fromIntegral h

  let (targetH, targetW) = (min tH (round $ fromIntegral h * ratioH), min tW (round $ ratioH * fromIntegral w * 2.0)) :: (Int, Int)

  let resizedImage = T.resize T.Bilinear (ix2 targetH targetW) currentFrame :: I.RGB
  let pixels = (chunksOf targetW . VV.toList . VV.map f . convert) (I.manifestVector resizedImage)

  -- difference between tty size and image
  let diffH = (tH - length pixels) `div` 2
  let diffW = (tW - targetW) `div` 2

  when supportsANSI $ setSGR [SetColor Background Dull Black]
  mapM_ (f'' diffW supportsANSI) pixels
  putStr $ replicate diffH '\n'
  _ <- threadDelay framePauseTime
  _ <- clearScreen
  processFrame env acc
processFrame (AppEnv { images = []}) _ = error "No frames provided!"
processFrame env@(AppEnv { images = allFrames, .. }) [] = do
  if shuffleFrames then do
    randomGen <- newStdGen
    processFrame env $ shuffle' allFrames frameAmount randomGen
  else processFrame env allFrames

runRunCommand :: AppOpts -> IO ()
runRunCommand (AppOpts { .. }) = let
  f :: DynamicImage -> Image PixelRGB8
  f (ImageRGB8 i) = i
  f otherImage    = convertRGB8 otherImage
  in do
  random <- newStdGen
  supportsANSI <- hNowSupportsANSI stdout
  frameFiles <- listDirectory framesPath
  let filteredNames = drop (startAt - 1) $ sortOn ((\x -> Text.Read.read x :: Int) . dropExtensions) $ filter (isJust . (\x -> readMaybe x :: Maybe Int) . dropExtensions) frameFiles
  imagesLoad <- mapM (readImage . (\x -> framesPath `combine` x)) filteredNames
  case sequence imagesLoad of
    (Left e) -> putStrLn $ "Image load error: " <> show e
    (Right images) -> do
      let convertedImages = map (toFridayRGB . f) images
      putStrLn "Loaded images!"
      _ <- clearScreen
      processFrame (AppEnv
        { supportsANSI = supportsANSI
        , images = convertedImages
        , framePauseTime = pauseTime
        , frameAmount = length convertedImages
        , shuffleFrames = shuffle
        }) []

runCommand :: AppOpts -> IO ()
runCommand opts@(AppOpts { appCommand = Run,.. }) = runRunCommand opts
