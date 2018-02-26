{-# LANGUAGE OverloadedStrings #-}

-- Spec: http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html#C.IDAT

module PNG where

import Data.List (intersperse)
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad (guard)
import Control.Applicative (many)

import Codec.Compression.Zlib (decompress)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

pngMagicHeader :: BS.ByteString
pngMagicHeader = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

data PNGChunk = PNGChunk
  { size     :: Word32
  , ctype    :: String
  , contents :: BS.ByteString
  , crc32    :: BS.ByteString
  }
  deriving (Show)

data PNGHeader = PNGHeader
  { width             :: Word32
  , height            :: Word32
  , depth             :: Word8
  , colorType         :: Word8
  , compressionMethod :: Word8
  , filterMethod      :: Word8
  , interlaceMethod   :: Word8
  }
  deriving (Show)

data PNG = PNG
  { header :: PNGHeader
  , chunks :: [PNGChunk]
  }
  deriving (Show)

getChunk :: Get PNGChunk
getChunk = do
  size <- getWord32be
  PNGChunk size
    <$> (BS.unpack <$> getByteString 4)
    <*> getByteString (fromIntegral size) 
    <*> getByteString 4

decodeChunk :: PNGChunk -> Get PNGChunk
decodeChunk chunk = return $ chunk { contents = BL.toStrict $ decompress (BL.fromStrict $ contents chunk) }

getHeader :: Get PNGHeader
getHeader = do
  width <- getWord32be
  height <- getWord32be
  depth <- getWord8
  colorType <- getWord8

  compression <- getWord8
  guard (compression == 0)

  filter <- getWord8
  guard (filter == 0)
  
  interlace <- getWord8
  guard (interlace == 0 || interlace == 1)

  return $ PNGHeader width height depth colorType compression filter interlace

getPNG :: Get PNG
getPNG = do
  magicHeader <- getByteString 8
  guard (magicHeader == pngMagicHeader)
  ch <- getChunk
  guard (size ch == 13)
  case runGetOrFail getHeader (BL.fromStrict $ contents ch) of
    Left (_, _, err) -> fail err
    Right (_, _, header) -> PNG header <$> many getChunk

decompressPNG :: [PNGChunk] -> BS.ByteString
decompressPNG = BL.toStrict . decompress . BL.fromStrict . foldr (BS.append . contents) ""

pngFromFile :: FilePath -> IO (Either String PNG)
pngFromFile path = do
  file <- BL.readFile path
  case runGetOrFail getPNG file of
    Left (_, _, err) -> return (Left err)
    Right (_, _, png) -> return (Right png)

data RGB = RGB
  { red   :: Integer
  , green :: Integer
  , blue  :: Integer
  }

getRGB :: Word8 -> Get RGB
getRGB depth = 
  RGB <$> rgbVal <*> rgbVal <*> rgbVal
  where
    rgbVal
      | depth == 8  = fromIntegral <$> getWord8
      | depth == 16 = fromIntegral <$> getWord16be
      | otherwise   = fail ("Unrecognized bit depth" ++ show depth)

blToRGB :: Word8 -> BL.ByteString -> Either String [RGB]
blToRGB depth bs = 
  case runGetOrFail (many $ getRGB depth) bs of
    Left (_, _, err)   -> Left err
    Right (_, _, rgbs) -> Right rgbs


putRGB :: RGB -> Put
putRGB rgb = do
  putWord32be (fromIntegral $ red rgb)
  putWord32be (fromIntegral $ green rgb)
  putWord32be (fromIntegral $ blue rgb)

rgbToBS :: RGB -> BL.ByteString
rgbToBS rgb = runPut (putRGB rgb)

rgbToRGBA :: [RGB] -> BL.ByteString
rgbToRGBA = foldr BL.append "" . intersperse (runPut $ putWord32be 0) . map rgbToBS
