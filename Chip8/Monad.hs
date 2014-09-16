module Chip8.Monad
  ( MonadEmulator (..)
  ) where

import Data.Word (Word8, Word16)

import Chip8.Emulator (Address8, Address16)

class (Functor m, Monad m) => MonadEmulator m where
    load8 :: Address8 -> m Word8
    store8 :: Address8 -> Word8 -> m ()
    load16 :: Address16 -> m Word16
    store16 :: Address16 -> Word16 -> m ()
