module Chip8.Memory where

import Data.Word (Word8)
import Data.Array.ST (STUArray)

import Chip8.Types

newtype Memory s = Memory { getArray :: STUArray s Address Word8 }

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
