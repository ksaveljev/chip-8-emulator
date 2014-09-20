import System.Environment (getArgs)
import qualified Data.ByteString as B

import Chip8.Emulator
import Chip8.Emulator.SDL
import Chip8.Emulator.Debug

debug :: IO ()
debug = do
    [romFilePath] <- getArgs
    romFile <- B.readFile romFilePath
    runDebugEmulator $ do
      loadProgram romFile
      emulate

sdl :: IO ()
sdl = do
    [romFilePath] <- getArgs
    romFile <- B.readFile romFilePath
    runSDLEmulator $ do
      loadProgram romFile
      emulate

main :: IO ()
main = sdl
