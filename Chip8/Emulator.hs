module Chip8.Emulator where

import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, (.&.))
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import Chip8.Monad
import Chip8.Memory (Address(..), MemoryValue(..), Register(..))
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
execute (SYS _) = return () -- This instruction is only used on the old computers on which Chip-8 was originally implemented. It is ignored by modern interpreters. 
execute CLS = clearScreen
execute RET = undefined
execute (JP (Ram addr)) = store Pc (MemoryValue16 addr)
execute (CALL (Ram addr)) = undefined
execute (SEB vx byte) = undefined
execute (SNEB vx byte) = undefined
execute (SER vx vy) = undefined
execute (LDB vx byte) = store (Register vx) (MemoryValue8 byte)
execute (ADDB vx byte) = do
    (MemoryValue8 v) <- load $ Register vx
    store (Register vx) (MemoryValue8 $ v + byte)
execute (LDR vx vy) = do
    v <- load $ Register vy
    store (Register vx) v
execute (OR vx vy) = undefined
execute (AND vx vy) = undefined
execute (XOR vx vy) = undefined
execute (ADDR vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    let c = fromIntegral a + fromIntegral b :: Word16
    store (Register VF) (MemoryValue8 $ if a + b > 255 then 1 else 0)
    store (Register vx) (MemoryValue8 $ fromIntegral $ c .&. 0xFFFF)
execute (SUB vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    let c = fromIntegral a - fromIntegral b :: Word16
    store (Register VF) (MemoryValue8 $ if a > b then 1 else 0)
    store (Register vx) (MemoryValue8 $ fromIntegral $ c .&. 0xFFFF)
execute (SHR vx) = do
    (MemoryValue8 v) <- load $ Register vx
    store (Register VF) (MemoryValue8 $ v .&. 0x1)
    store (Register vx) (MemoryValue8 $ v `shiftR` 1)
execute (SUBN vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    let c = fromIntegral b - fromIntegral a :: Word16
    store (Register VF) (MemoryValue8 $ if b > a then 1 else 0)
    store (Register vx) (MemoryValue8 $ fromIntegral $ c .&. 0xFFFF)
execute (SHL vx) = do
    (MemoryValue8 v) <- load $ Register vx
    store (Register VF) (MemoryValue8 $ (v `shiftR` 7) .&. 0x1)
    store (Register vx) (MemoryValue8 $ v `shiftL` 1)
execute (SNER vx vy) = undefined
execute (LDI (Ram addr)) = store (Register I) (MemoryValue16 addr)
execute (LONGJP (Ram addr)) = do
    (MemoryValue8 v) <- load $ Register V0
    store Pc (MemoryValue16 $ fromIntegral v + addr)
execute (RND vx byte) = undefined
execute (DRW vx vy nibble) = undefined
execute (SKP vx) = undefined
execute (SKNP vx) = undefined
execute (LDRDT vx) = do
    v <- load $ Register DT
    store (Register vx) v
execute (LDK vx) = undefined
execute (LDDTR vx) = do
    v <- load $ Register vx
    store (Register DT) v
execute (LDST vx) = do
    v <- load $ Register vx
    store (Register ST) v
execute (ADDI vx) = do
    (MemoryValue16 a) <- load $ Register I
    (MemoryValue8 b) <- load $ Register vx
    store (Register I) (MemoryValue16 $ a + fromIntegral b)
execute (LDF vx) = do
    (MemoryValue8 v) <- load $ Register vx
    store (Register I) (MemoryValue16 $ fromIntegral $ v .&. 0xF)
execute (LDBCD vx) = undefined
execute (LDIR vx) = undefined
execute (LDRI vx) = undefined
execute _ = return ()
