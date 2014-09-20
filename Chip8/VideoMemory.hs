module Chip8.VideoMemory where

import Data.Word (Word8)
import Data.Bits (testBit)
import Control.Monad (foldM, when)
import Control.Monad.ST (ST)
import qualified Data.Array.BitArray.ST as BA

import Debug.Trace

type VideoMemory s = BA.STBitArray s Int

vScale :: Int
vScale = 8

vWidth :: Int
vWidth = 64

vHeight :: Int
vHeight = 32

newVideoMemory :: ST s (VideoMemory s)
newVideoMemory = BA.newArray (0, vWidth * vHeight - 1) False

clearVideoMemory :: VideoMemory s -> ST s ()
clearVideoMemory vram = BA.fill vram False

draw :: VideoMemory s -> Word8 -> Word8 -> [Word8] -> ST s Bool
draw vram x y sprite = do
      let bools = concatMap toBoolList sprite
      (_, erased) <- foldM setPixel (0, False) bools
      return erased
        where
          posIndex px py = (py `mod` vHeight) * vWidth + (px `mod` vWidth)
          drawPixel vx vy = BA.writeArray vram (posIndex vx vy)
          setPixel (a, e) state = do
            let dx = a `mod` 8 :: Int
            let dy = a `div` 8 :: Int
            currentStateOn <- BA.readArray vram (posIndex (fromIntegral x + dx) (fromIntegral y + dy))
            let e' = state /= currentStateOn
            when e' $ drawPixel (fromIntegral x + dx) (fromIntegral y + dy) state
            return (a + 1, e' || e)

toBoolList :: Word8 -> [Bool]
toBoolList w8 = reverse . toBoolList' w8 $ 0
  where
    toBoolList' _ 8 = []
    toBoolList' w b = testBit w b : toBoolList' w (b + 1)
