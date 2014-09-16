module Chip8.Emulator where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as B

import Chip8.Monad
import Chip8.Memory
import Chip8.Registers

data Register = V1 | V2 | V3 | V4 | V5
              | V6 | V7 | V8 | V9 | VA
              | VB | VC | VD | VE | VF

data Address8 = Register Register
              | DelayTimer
              | SoundTimer
              | Sp
              | Ram Word16

data Address16 = I 
               | Pc 
               | Stack

data Emulator s = Emulator { memory :: Memory s
                           , registers :: Registers s
                           , registerI :: Word16
                           , delayTimer :: Word8
                           , soundTimer :: Word8
                           , pc :: Word16
                           , sp :: Word8
                           , stack :: [Word16]
                           }

