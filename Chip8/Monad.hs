module Chip8.Monad
  ( MonadEmulator (..)
  ) where

import Data.Word (Word8)

import Chip8.Memory (Address, MemoryValue)
import Chip8.Event (Key)

class (Functor m, Monad m) => MonadEmulator m where
    load :: Address -> m MemoryValue
    store :: Address -> MemoryValue -> m ()
    clearScreen :: m ()
    drawSprite :: Word8 -> Word8 -> Int -> Address -> m Bool
    randomWord8 :: m Word8
    handleEvents :: m ()
    waitForKeyPress :: m Key
    isKeyPressed :: Key -> m Bool
    sleep :: m ()
    isDone :: m Bool
