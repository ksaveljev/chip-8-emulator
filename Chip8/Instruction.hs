module Chip8.Instruction where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR)

import Chip8.Memory (Register(..), Address(..), toRegister)

data Instruction = SYS    Address                 -- Jump to a machine code routine at Address
                 | CLS                            -- Clear the display
                 | RET                            -- Return from a subroutine
                 | JP     Address                 -- Jump to location Address
                 | CALL   Address                 -- Call subroutine at Address
                 | SEB    Register Word8          -- Skip next instruction if Register value == Word8
                 | SNEB   Register Word8          -- Skip next instruction if Register value != Word8
                 | SER    Register Register       -- Skip next instruction if registers contain same value
                 | LDB    Register Word8          -- Set Register value to Word8
                 | ADDB   Register Word8          -- Add Word8 to Register value
                 | LDR    Register Register       -- Copy second Register value to first Register
                 | OR     Register Register       -- Set first Register value to bitwise OR of both register values
                 | AND    Register Register       -- Set first Register value to bitwise AND of both register values
                 | XOR    Register Register       -- Set first Register value to bitwise XOR of both register values
                 | ADDR   Register Register       -- Second Register value is added to first Register value
                 | SUB    Register Register       -- Second Register value is subtracted from first Register value
                 | SHR    Register                -- Divide Register value by 2
                 | SUBN   Register Register       -- First Register value is subtracted from second Register value, result stored in first Register value
                 | SHL    Register                -- Multiply Register value by 2
                 | SNER   Register Register       -- Skip next instruction if registers contain different values
                 | LDI    Address                 -- Set register I value to Address
                 | LONGJP Address                 -- Jump to location Address
                 | RND    Register Word8          -- Set register value to bitwise AND of random byte and Word8
                 | DRW    Register Register Word8 -- Display sprite at memory location I at (Register, Register)
                 | SKP    Register                -- Skip next instruction if key with the value of Register is pressed
                 | SKNP   Register                -- Skip next instruction if key with the value of Register is not pressed
                 | LDRDT  Register                -- Set Register value to delay timer value
                 | LDK    Register                -- Wait for a key press, store the value of the key in Register
                 | LDDTR  Register                -- Set delay time to Register value
                 | LDST   Register                -- Set sound timer to Register value
                 | ADDI   Register                -- Add Register value to I register value
                 | LDF    Register                -- Set I to the location of sprite for digit stored in Register
                 | LDBCD  Register                -- Store BCD representation of Register in memory locations I, I+1, I+2
                 | LDIR   Register                -- Store registers V0 through Register in memory starting at location I
                 | LDRI   Register                -- Read registers V0 through Register from memory starting at location I
                 deriving (Show)

decodeInstruction :: Word16 -> Instruction
decodeInstruction instruction =
    case operation of
      0x0 -> case instruction of
               0x00E0 -> CLS
               0x00EE -> RET
               _ -> SYS (Ram addr)
      0x1 -> JP (Ram addr)
      0x2 -> CALL (Ram addr)
      0x3 -> SEB vx byte
      0x4 -> SNEB vx byte
      0x5 -> case op of
               0x0 -> SER vx vy
               _ -> oops
      0x6 -> LDB vx byte
      0x7 -> ADDB vx byte
      0x8 -> case op of
               0x0 -> LDR vx vy
               0x1 -> OR vx vy
               0x2 -> AND vx vy
               0x3 -> XOR vx vy
               0x4 -> ADDR vx vy
               0x5 -> SUB vx vy
               0x6 -> SHR vx
               0x7 -> SUBN vx vy
               0xE -> SHL vx
               _ -> oops
      0x9 -> case op of
               0x0 -> SNER vx vy
               _ -> oops
      0xA -> LDI (Ram addr)
      0xB -> LONGJP (Ram addr)
      0xC -> RND vx byte
      0xD -> DRW vx vy (fromIntegral op)
      0xE -> case xoxx of
               0xE090 -> SKP vx
               0xE0A1 -> SKNP vx
               _ -> oops
      0xF -> case xoxx of
               0xF007 -> LDRDT vx
               0xF00A -> LDK vx
               0xF015 -> LDDTR vx
               0xF018 -> LDST vx
               0xF01E -> ADDI vx
               0xF029 -> LDF vx
               0xF033 -> LDBCD vx
               0xF055 -> LDIR vx
               0xF065 -> LDRI vx
               _ -> oops
      _ -> oops
    where
      operation = (instruction .&. 0xF000) `shiftR` 12
      addr = instruction .&. 0x0FFF
      byte = fromIntegral (instruction .&. 0x00FF)
      op = instruction .&. 0x000F
      vx = toRegister $ (instruction .&. 0x0F00) `shiftR` 8
      vy = toRegister $ (instruction .&. 0x00F0) `shiftR` 4
      xoxx = instruction .&. 0xF0FF
      oops = error "Unknown instruction"
