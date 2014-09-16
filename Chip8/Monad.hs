module Chip8.Monad
  ( MonadEmulator (..)
  ) where

import Chip8.Memory (Address, MemoryValue)

class (Functor m, Monad m) => MonadEmulator m where
    load :: Address -> m MemoryValue
    store :: Address -> MemoryValue -> m ()
