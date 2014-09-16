module Chip8.Memory where

import Data.Word (Word8, Word16)
import Data.Array.ST (STUArray, newArray)
import Control.Monad.ST (ST)

data Address = Register Register
             | Pc
             | Sp
             | Ram Word16

data Register = V1 | V2 | V3 | V4 | V5
              | V6 | V7 | V8 | V9 | VA
              | VB | VC | VD | VE | VF
              | DT | ST | I

data MemoryValue = MemoryValue8 Word8
                 | MemoryValue16 Word16

data Memory s = Memory { memory :: STUArray s Word16 Word8
                       , registers :: STUArray s Word8 Word8
                       , registerI :: Word16
                       , delayTimer :: Word8
                       , soundTimer :: Word8
                       , pc :: Word16
                       , sp :: Word8
                       , stack :: STUArray s Word8 Word16
                       }

new :: ST s (Memory s)
new = do
    memory' <- newArray (0x000, 0xFFF) 0
    registers' <- newArray (0x0, 0xF) 0
    stack' <- newArray (0x0, 0xF) 0
    return Memory { memory = memory'
                  , registers = registers'
                  , registerI = 0
                  , delayTimer = 0
                  , soundTimer = 0
                  , pc = 0x200
                  , sp = 0
                  , stack = stack'
                  }

store :: Memory s -> Address -> MemoryValue -> ST s ()
store = undefined

load :: Memory s -> Address -> ST s MemoryValue
load = undefined

font :: [Word8]
font = [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
       , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
       , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
       , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
       , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
       , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
       , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
       , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
       , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
       , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
       , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
       , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
       , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
       , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
       , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
       , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
       ]
