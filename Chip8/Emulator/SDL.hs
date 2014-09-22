{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chip8.Emulator.SDL ( runSDLEmulator
                          ) where

import Data.Word (Word32)
import Data.List (foldl')
import Data.Bits ((.|.))
import Data.STRef (readSTRef, writeSTRef)
import Control.Monad (foldM_)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.ST (getElems)
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
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with, maybePeek)
import Foreign.Storable (peek)

import Chip8.Monad
import Chip8.VideoMemory (VideoMemory, clearVideoMemory, draw, vScale, vWidth, vHeight)
import Chip8.Event (EventState(..), Key(..))
import Chip8.Memory (Memory(..), Address(..))
import qualified Chip8.Memory as Memory
import qualified Chip8.Event as Event

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
      lift $ stToIO $ draw (videoMemory mem) x y sprite
    drawSprite _ _ _ _ = error "Incorrect Address in drawSprite"
    randomWord8 = SDLEmulator $ do
      mem <- ask
      g <- lift $ stToIO $ readSTRef $ stdGen mem
      let (rnd, g') = randomR (0x0, 0xFF) g
      lift $ stToIO $ writeSTRef (stdGen mem) g'
      return rnd 
    handleEvents = SDLEmulator $ do
      mem <- ask
      let kes = eventState mem
      lift $ handleNextEvent kes
    waitForKeyPress = SDLEmulator $ do
      mem <- ask
      lift $ getKey (eventState mem)
      where
        getKey kes = do
          handleNextEvent kes
          k <- stToIO $ readSTRef $ lastEventKeyDown kes
          case k of
            Nothing -> getKey kes
            Just key -> return key
    isKeyPressed key = SDLEmulator $ do
      mem <- ask
      lift $ stToIO $ Event.isKeyPressed (eventState mem) key
    sleep = SDLEmulator $ lift $ threadDelay 800
    isDone = SDLEmulator $ do
      mem <- ask
      lift $ stToIO $ Event.isTimeToQuit (eventState mem)

drawVideoMemory :: SDL.Renderer -> VideoMemory RealWorld -> IO ()
drawVideoMemory renderer vram = do
    vm <- stToIO $ BA.getElems vram
    drawVM vm
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
    with shape $ SDL.renderFillRect renderer

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pointer -> do
  status <- SDL.pollEvent pointer
  if status == 1
    then maybePeek peek pointer
    else return Nothing

handleNextEvent :: EventState RealWorld -> IO ()
handleNextEvent es = do
    maybeEvent <- pollEvent
    case maybeEvent of
      Nothing -> return ()
      Just (SDL.QuitEvent _ _) -> stToIO $ Event.timeToQuit es
      Just (SDL.KeyboardEvent eventType _ _ _ _ keysym) ->
        case eventType of
          768 -> do -- SDL_KEYDOWN
            stToIO $ Event.setLastKeyPressed es (keymap keysym)
            setKeyHandler keysym True
          769 -> do -- SDL_KEYUP
            stToIO $ Event.setLastKeyPressed es Nothing
            setKeyHandler keysym False
          _ -> stToIO $ Event.setLastKeyPressed es Nothing
        where
          setKeyHandler key b =
            case keymap key of
              Just k -> stToIO $ Event.setKey es k b
              _ -> return ()
      _ -> return ()

keymap :: SDL.Keysym -> Maybe Key
keymap (SDL.Keysym keysymScancode _ _) =
    case keysymScancode of
      36 -> Just K1 -- 7
      37 -> Just K2 -- 8
      38 -> Just K3 -- 9
      39 -> Just KC -- 0
      24 -> Just K4 -- u
      12 -> Just K5 -- i
      18 -> Just K6 -- o
      19 -> Just KD -- p
      13 -> Just K7 -- j
      14 -> Just K8 -- k
      15 -> Just K9 -- l
      51 -> Just KE -- ;
      16 -> Just KA -- m
      54 -> Just K0 -- ,
      55 -> Just KB -- .
      56 -> Just KF -- /
      _ -> Nothing

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
