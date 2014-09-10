import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

main :: IO()
main = do
    [romFilePath] <- getArgs
    romFile <- B.readFile romFilePath
    -- initialize emulator from romFile
    undefined
