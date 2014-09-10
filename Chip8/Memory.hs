module Chip8.Memory where

import Data.Word (Word8)
import Data.Array.ST (STUArray)

import Chip8.Types

newtype Memory s = Memory { getArray :: STUArray s Address Word8 }
