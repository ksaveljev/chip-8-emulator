module Chip8.Emulator where

import Data.Word (Word8)

import Chip8.Types
import Chip8.Memory
import Chip8.Registers


data Emulator s = Emulator { memory :: Memory s
                           , registers :: Registers s
                           , registerI :: Address
                           , delayTimer :: Word8
                           , soundTimer :: Word8
                           , pc :: Address
                           , sp :: Word8
                           , stack :: [Address]
                           }
