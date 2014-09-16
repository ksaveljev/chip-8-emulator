module Chip8.Emulator where

import qualified Data.ByteString as B

import Chip8.Monad

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram = undefined
