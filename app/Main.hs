module Main where

import qualified Data.ByteString.Lazy as BL
import Display
import PNG

filename :: FilePath
filename = "test.png"

main :: IO ()
main = do
  file <- pngFromFile filename
  case file of
    Left err -> print err
    Right png ->
      case blToRGB (d png) (BL.fromStrict . decompressPNG . chunks $ png) of
        Left err -> print err
        Right rgbs -> renderBitmap filename (w png, h png) (BL.toStrict $ rgbToRGBA rgbs)
  where
    w = fromIntegral . width . header
    h = fromIntegral . height . header
    d = depth . header

