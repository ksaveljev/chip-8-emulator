module Chip8.Emulator where

import qualified Data.ByteString as B

import Chip8.Monad
import Chip8.Memory (Address(..), MemoryValue(..))

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program =
    mapM_ loadByte [0..B.length program - 1]
    where
      offset = 0x200
      loadByte i = 
        let byte = B.index program i
            address = offset + fromIntegral i
        in store (Ram address) (MemoryValue8 byte)
