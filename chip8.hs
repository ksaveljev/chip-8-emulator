import System.Environment (getArgs)
import Graphics.UI.SDL as SDL
import qualified Data.ByteString as B

import Chip8.Emulator
import Chip8.Monad.IO

main :: IO()
main = do
    SDL.init [InitVideo]
    [romFilePath] <- getArgs
    romFile <- B.readFile romFilePath
    runIOEmulator $ do
      loadProgram romFile
      emulate
