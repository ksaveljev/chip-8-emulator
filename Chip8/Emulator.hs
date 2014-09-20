module Chip8.Emulator where

import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Control.Monad (when, unless)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import Chip8.Monad
import Chip8.Memory (Address(..), MemoryValue(..), Register(..))
import Chip8.Instruction

emulate :: MonadEmulator m => m ()
emulate = do
    instr <- loadInstruction
    execute instr
    sleep
    emulate

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
    incrementProgramCounter
    return $ (fromIntegral msbyte `shiftL` 8) + fromIntegral lsbyte

loadInstruction :: MonadEmulator m => m Instruction
loadInstruction = decodeInstruction <$> loadNextWord

incrementProgramCounter :: MonadEmulator m => m ()
incrementProgramCounter = do
    (MemoryValue16 v) <- load Pc
    store Pc (MemoryValue16 $ v + 0x2)

execute :: MonadEmulator m => Instruction -> m ()
execute (SYS _) = return () -- This instruction is only used on the old computers on which Chip-8 was originally implemented. It is ignored by modern interpreters. 
execute CLS = clearScreen
execute RET = do
    addr <- load Stack
    store Pc addr
execute (JP (Ram addr)) = store Pc (MemoryValue16 addr)
execute (CALL (Ram addr)) = do
    pc' <- load Pc
    store Stack pc'
    store Pc (MemoryValue16 addr)
execute (SEB vx byte) = do
    (MemoryValue8 v) <- load $ Register vx
    when (v == byte) incrementProgramCounter
execute (SNEB vx byte) = do
    (MemoryValue8 v) <- load $ Register vx
    when (v /= byte) incrementProgramCounter
execute (SER vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    when (a == b) incrementProgramCounter
execute (LDB vx byte) = store (Register vx) (MemoryValue8 byte)
execute (ADDB vx byte) = do
    (MemoryValue8 v) <- load $ Register vx
    store (Register vx) (MemoryValue8 $ v + byte)
execute (LDR vx vy) = do
    v <- load $ Register vy
    store (Register vx) v
execute (OR vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    store (Register vx) (MemoryValue8 $ a .|. b)
execute (AND vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    store (Register vx) (MemoryValue8 $ a .&. b)
execute (XOR vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    store (Register vx) (MemoryValue8 $ a `xor` b)
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
execute (SNER vx vy) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    when (a /= b) incrementProgramCounter
execute (LDI (Ram addr)) = store (Register I) (MemoryValue16 addr)
execute (LONGJP (Ram addr)) = do
    (MemoryValue8 v) <- load $ Register V0
    store Pc (MemoryValue16 $ fromIntegral v + addr)
execute (RND vx byte) = do
    rnd <- randomWord8
    store (Register vx) (MemoryValue8 $ rnd .&. byte)
execute (DRW vx vy nibble) = do
    (MemoryValue8 a) <- load $ Register vx
    (MemoryValue8 b) <- load $ Register vy
    (MemoryValue16 i) <- load $ Register I
    collision <- drawSprite a b (fromIntegral nibble) (Ram i)
    store (Register VF) (MemoryValue8 $ if collision then 1 else 0)
execute (SKP vx) = do
    (MemoryValue8 v) <- load $ Register vx
    pressed <- isKeyPressed $ toEnum $ fromIntegral v
    when pressed incrementProgramCounter
execute (SKNP vx) = do
    (MemoryValue8 v) <- load $ Register vx
    pressed <- isKeyPressed $ toEnum $ fromIntegral v
    unless pressed incrementProgramCounter
execute (LDRDT vx) = do
    v <- load $ Register DT
    store (Register vx) v
execute (LDK vx) = do
    key <- waitForKeyPress
    store (Register vx) (MemoryValue8 $ fromIntegral $ fromEnum key)
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
execute (LDBCD vx) = do
    (MemoryValue8 v) <- load $ Register vx
    (MemoryValue16 i) <- load $ Register I
    let ones = v `mod` 10
    let tens = (v `div` 10) `mod` 10
    let hundreds = v `div` 100
    store (Ram i) (MemoryValue8 hundreds)
    store (Ram $ i + 1) (MemoryValue8 tens)
    store (Ram $ i + 2) (MemoryValue8 ones)
execute (LDIR vx) = do
    (MemoryValue16 i) <- load $ Register I
    mapM_ (\r -> do
            v <- load $ Register r
            store (Ram $ i + fromIntegral (fromEnum r)) v
          ) [V0 .. vx]
execute (LDRI vx) = do
    (MemoryValue16 i) <- load $ Register I
    mapM_ (\r -> do
            v <- load $ Ram (i + fromIntegral (fromEnum r))
            store (Register r) v
          ) [V0 .. vx]
execute _ = return ()
