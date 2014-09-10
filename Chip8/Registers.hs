module Chip8.Registers where

import Data.Word (Word8)
import Data.Array.ST (STUArray)

newtype Registers s = Registers { getArray :: STUArray s Word8 Word8 }
