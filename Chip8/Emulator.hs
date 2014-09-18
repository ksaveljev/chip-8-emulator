module Chip8.Emulator where

import Data.Word (Word16)
import Data.Bits (shiftL)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import Chip8.Monad
import Chip8.Memory (Address(..), MemoryValue(..))
import Chip8.Instruction

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program =
    mapM_ loadByte [0..B.length program - 1]
    where
      offset = 0x200
      loadByte i = 
        let byte = B.index program i
            address = offset + fromIntegral i
        in store (Ram address) (MemoryValue8 byte)

loadNextWord :: MonadEmulator m => m Word16
loadNextWord = do
    (MemoryValue16 pc) <- load Pc
    (MemoryValue8 msbyte) <- load (Ram pc)
    (MemoryValue8 lsbyte) <- load (Ram $ pc + 1)
    store Pc $ MemoryValue16 (pc + 2)
    return $ fromIntegral (msbyte `shiftL` 8) + fromIntegral lsbyte

loadInstruction :: MonadEmulator m => m Instruction
loadInstruction = decodeInstruction <$> loadNextWord
