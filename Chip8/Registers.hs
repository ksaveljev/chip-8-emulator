module Chip8.Registers where

import Data.Word (Word8)
import Data.Array.ST (STUArray)

data Register = V1 | V2 | V3 | V4 | V5
              | V6 | V7 | V8 | V9 | VA
              | VB | VC | VD | VE | VF
              | DT | ST | I

newtype Registers s = Registers { getArray :: STUArray s Word8 Word8 }
