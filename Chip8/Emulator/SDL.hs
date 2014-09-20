{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chip8.Emulator.SDL ( runSDLEmulator
                          ) where

import Data.Word (Word32)
import Data.List (foldl')
import Data.Bits ((.|.))
import Data.STRef (readSTRef, writeSTRef)
import Control.Monad (foldM_)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.ST (readArray, getElems)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Control.Applicative
import System.Random (newStdGen, randomR)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Suspend.Lifted
import Control.Concurrent.Timer
import qualified Data.Array.BitArray.ST as BA

import qualified Graphics.UI.SDL as SDL
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils (with)

import Chip8.Monad
import Chip8.VideoMemory (VideoMemory, clearVideoMemory, draw, vScale, vWidth, vHeight)
import Chip8.KeyEvent (KeyEventState(..), Key(..))
import Chip8.Memory (Memory(..), Address(..))
import qualified Chip8.Memory as Memory

import Debug.Trace

newtype SDLEmulator a = SDLEmulator (ReaderT (Memory RealWorld) IO a)
                        deriving (Functor, Applicative, Monad, MonadIO)

type Risky a = Either String a

data Colour = Black | White deriving Show

screenWidth :: CInt
screenWidth = fromIntegral vWidth

screenHeight :: CInt
screenHeight = fromIntegral vHeight

screenScale :: CInt
screenScale = fromIntegral vScale

instance MonadEmulator SDLEmulator where
    load address = SDLEmulator $ do
      mem <- ask
      -- requires import of Debug.Trace
      -- value <- lift $ stToIO $ Memory.load mem address
      -- trace ("load from " ++ (show address) ++ " value " ++ (show value)) $ lift $ stToIO $ Memory.load mem address
      lift $ stToIO $ Memory.load mem address
    store address value = SDLEmulator $ do
      mem <- ask
      lift $ stToIO $ Memory.store mem address value
    clearScreen = SDLEmulator $ do
      mem <- ask
      lift $ stToIO $ clearVideoMemory (videoMemory mem)
    drawSprite x y n (Ram address) = SDLEmulator $ do
      mem <- ask
      sprite <- lift $ stToIO $ fmap (take n . drop (fromIntegral address)) (getElems $ memory mem)
      trace ("address: " ++ (show address) ++ " sprite: " ++ (show sprite)) $ lift $ stToIO $ draw (videoMemory mem) x y sprite
    drawSprite _ _ _ _ = error "Incorrect Address in drawSprite"
    randomWord8 = SDLEmulator $ do
      mem <- ask
      g <- lift $ stToIO $ readSTRef $ stdGen mem
      let (rnd, g') = randomR (0x0, 0xFF) g
      lift $ stToIO $ writeSTRef (stdGen mem) g'
      return rnd 
    waitForKeyPress = SDLEmulator $ do
      lift $ putStrLn "HI"
      return K0 -- TODO: undefined
    isKeyPressed key = SDLEmulator $ do
      mem <- ask
      lift $ stToIO $ readArray (keyState $ keyEventState mem) key
    sleep = SDLEmulator $ lift $ threadDelay 800

drawVideoMemory :: SDL.Renderer -> VideoMemory RealWorld -> IO ()
drawVideoMemory renderer vram = do
    vm <- stToIO $ BA.getElems vram
    trace ("draaaaw memory! " ++ (show $ length vm)) $ drawVM vm
      where
        pixel x y = SDL.Rect (screenScale * fromIntegral x) (screenScale * fromIntegral y) screenScale screenScale
        drawVM mem = do
          foldM_ drawBit 0 mem
          SDL.renderPresent renderer
        drawBit a i = do
          let x = a `mod` vWidth
          let y = a `div` vWidth
          _ <- fillRectangle renderer (pixel x y) (if i then White else Black)
          return $ a + 1

initializeSDL :: [Word32] -> IO (Risky())
initializeSDL flags = do
    initiSuccess <- SDL.init $ foldl' (.|.) 0 flags
    return $ if initiSuccess < 0 then Left "SDL could not initialize!" else Right ()

catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return

throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)

createWindow :: String -> IO (Risky SDL.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined (screenWidth * screenScale) (screenHeight * screenScale) SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window

createRenderer :: SDL.Window -> CInt -> [Word32] -> IO (Risky SDL.Renderer)
createRenderer window index flags = do
    renderer <- SDL.createRenderer window index $ foldl' (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer

clearWindow :: SDL.Renderer -> IO CInt
clearWindow renderer = do
    _ <- setColor renderer Black
    SDL.renderClear renderer

setColor :: SDL.Renderer -> Colour -> IO CInt
setColor renderer White = SDL.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Black = SDL.setRenderDrawColor renderer 0x00 0x00 0x00 0x00

fillRectangle :: SDL.Renderer -> SDL.Rect -> Colour -> IO CInt
fillRectangle renderer shape color = do
    _ <- setColor renderer color
    --trace ("fill " ++ show shape ++ " color = " ++ show color) $ with shape $ SDL.renderFillRect renderer
    with shape $ SDL.renderFillRect renderer

runSDLEmulator :: SDLEmulator a -> IO ()
runSDLEmulator (SDLEmulator reader) = do
    initializeSDL [SDL.initFlagVideo] >>= catchRisky
    window <- createWindow "CHIP-8 Emulator" >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.rendererFlagAccelerated] >>= catchRisky
    _ <- clearWindow renderer
    g <- newStdGen
    mem <- stToIO $ Memory.new g
    _ <- repeatedTimer (drawVideoMemory renderer (videoMemory mem)) (usDelay 100)
    _ <- runReaderT reader mem
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
