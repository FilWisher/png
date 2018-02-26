{-# LANGUAGE OverloadedStrings #-}

module Display where

import Graphics.Gloss (Display(..), white, circle, Color, Picture(..), display, bitmapOfByteString)
import Graphics.Gloss.Data.Bitmap (BitmapFormat(..), RowOrder(..), PixelFormat(..))
import PNG
import qualified Data.ByteString.Char8 as BS

window :: String -> (Int, Int) -> Display
window name pos = InWindow name pos (0, 0)

background :: Color
background = white

fromBitmap :: (Int, Int) -> BS.ByteString -> Picture
fromBitmap (w, h) bitmap = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bitmap True

renderBitmap :: FilePath -> (Int, Int) -> BS.ByteString -> IO ()
renderBitmap name dimensions bytes = display (window name dimensions) background (fromBitmap dimensions bytes)
