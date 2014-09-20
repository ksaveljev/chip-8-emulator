module Chip8.Memory where

import Data.Word (Word8, Word16)
import Data.STRef
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import System.Random (StdGen)
import Control.Monad.ST (ST)

import Chip8.KeyEvent
import Chip8.VideoMemory (VideoMemory, newVideoMemory)

data Address = Register Register
             | Pc
             | Sp
             | Stack
             | Ram Word16
             deriving (Show)

data Register = V0 | V1 | V2 | V3
              | V4 | V5 | V6 | V7
              | V8 | V9 | VA | VB
              | VC | VD | VE | VF
              | DT | ST | I
              deriving (Enum, Show)

toRegister :: Integral a => a -> Register
toRegister = toEnum . fromIntegral

data MemoryValue = MemoryValue8 Word8
                 | MemoryValue16 Word16
                 deriving (Show)

data Memory s = Memory { memory :: STUArray s Word16 Word8
                       , registers :: STUArray s Word8 Word8
                       , registerI :: STRef s Word16
                       , delayTimer :: STRef s Word8
                       , soundTimer :: STRef s Word8
                       , pc :: STRef s Word16
                       , sp :: STRef s Word8
                       , stack :: STUArray s Word8 Word16
                       , keyEventState :: KeyEventState s
                       , videoMemory :: VideoMemory s
                       , stdGen :: STRef s StdGen
                       }

new :: StdGen -> ST s (Memory s)
new rndGen = do
    memory' <- newArray (0x000, 0xFFF) 0
    registers' <- newArray (0x0, 0xF) 0
    registerI' <- newSTRef 0
    delayTimer' <- newSTRef 0
    soundTimer' <- newSTRef 0
    pc' <- newSTRef 0x200
    sp' <- newSTRef 0
    stack' <- newArray (0x0, 0xF) 0
    keyEventState' <- newKeyEventState
    videoMemory' <- newVideoMemory
    stdGen' <- newSTRef rndGen
    return Memory { memory = memory'
                  , registers = registers'
                  , registerI = registerI'
                  , delayTimer = delayTimer'
                  , soundTimer = soundTimer'
                  , pc = pc'
                  , sp = sp'
                  , stack = stack'
                  , keyEventState = keyEventState'
                  , videoMemory = videoMemory'
                  , stdGen = stdGen'
                  }

store :: Memory s -> Address -> MemoryValue -> ST s ()
store mem Pc            (MemoryValue16 v) = writeSTRef (pc mem) v
store mem Sp            (MemoryValue8 v)  = writeSTRef (sp mem) v
store mem (Register I)  (MemoryValue16 v) = writeSTRef (registerI mem) v
store mem (Register DT) (MemoryValue8 v)  = writeSTRef (delayTimer mem) v
store mem (Register ST) (MemoryValue8 v)  = writeSTRef (soundTimer mem) v
store mem (Register r)  (MemoryValue8 v)  = writeArray (registers mem) (fromIntegral $ fromEnum r) v
store mem (Ram addr)    (MemoryValue8 v)  = writeArray (memory mem) addr v
store mem Stack         (MemoryValue16 v) = do
    (MemoryValue8 sp') <- load mem Sp
    store mem Sp (MemoryValue8 $ sp' + 1)
    writeArray (stack mem) sp' v
store _ _ _ = error "Incorrect Memory 'store' command"

load :: Memory s -> Address -> ST s MemoryValue
load mem Pc            = readSTRef (pc mem) >>= \v -> return $ MemoryValue16 v
load mem Sp            = readSTRef (sp mem) >>= \v -> return $ MemoryValue8 v
load mem (Register I)  = readSTRef (registerI mem) >>= \v -> return $ MemoryValue16 v
load mem (Register DT) = readSTRef (delayTimer mem) >>= \v -> return $ MemoryValue8 v
load mem (Register ST) = readSTRef (soundTimer mem) >>= \v -> return $ MemoryValue8 v
load mem (Register r)  = readArray (registers mem) (fromIntegral $ fromEnum r) >>= \v -> return $ MemoryValue8 v
load mem (Ram addr)    = readArray (memory mem) addr >>= \v -> return $ MemoryValue8 v
load mem Stack         = do
    (MemoryValue8 sp') <- load mem Sp
    store mem Sp (MemoryValue8 $ sp' - 1)
    readArray (stack mem) (sp' - 1) >>= \v -> return $ MemoryValue16 v



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
