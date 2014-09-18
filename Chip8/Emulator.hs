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

execute :: MonadEmulator m => Instruction -> m ()
execute (SYS _) = return ()
execute CLS = undefined
execute RET = undefined
execute (JP (Ram addr)) = undefined
execute (CALL (Ram addr)) = undefined
execute (SEB vx byte) = undefined
execute (SNEB vx byte) = undefined
execute (SER vx vy) = undefined
execute (LDB vx byte) = undefined
execute (ADDB vx byte) = undefined
execute (LDR vx vy) = undefined
execute (OR vx vy) = undefined
execute (AND vx vy) = undefined
execute (XOR vx vy) = undefined
execute (ADDR vx vy) = undefined
execute (SUB vx vy) = undefined
execute (SHR vx) = undefined
execute (SUBN vx vy) = undefined
execute (SHL vx) = undefined
execute (SNER vx vy) = undefined
execute (LDI (Ram addr)) = undefined
execute (LONGJP (Ram addr)) = undefined
execute (RND vx byte) = undefined
execute (DRW vx vy nibble) = undefined
execute (SKP vx) = undefined
execute (SKNP vx) = undefined
execute (LDRDT vx) = undefined
execute (LDK vx) = undefined
execute (LDDTR vx) = undefined
execute (LDST vx) = undefined
execute (ADDI vx) = undefined
execute (LDF vx) = undefined
execute (LDBCD vx) = undefined
execute (LDIR vx) = undefined
execute (LDRI vx) = undefined
execute _ = return ()
