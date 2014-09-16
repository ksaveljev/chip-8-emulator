module Chip8.Emulator where

import Data.Word (Word8, Word16)

import Chip8.Memory
import Chip8.Registers

data Emulator s = Emulator { memory :: Memory s
                           , registers :: Registers s
                           , registerI :: Word16
                           , delayTimer :: Word8
                           , soundTimer :: Word8
                           , pc :: Word16
                           , sp :: Word8
                           , stack :: [Word16]
                           }

