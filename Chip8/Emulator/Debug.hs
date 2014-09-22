{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chip8.Emulator.Debug where

import Debug.Trace
import Data.STRef (readSTRef, writeSTRef)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.ST (readArray)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Control.Applicative
import System.Random (newStdGen, randomR)
import qualified Data.Array.BitArray.ST as BA

import Chip8.Monad
import Chip8.Event (EventState(..), Key(..))
import Chip8.Memory (Memory(..), Address(..))
import qualified Chip8.Memory as Memory

newtype DebugEmulator a = DebugEmulator (ReaderT (Memory RealWorld) IO a)
                       deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEmulator DebugEmulator where
    load address = DebugEmulator $ do
      trace ("Loading value from address " ++ show address) $ return ()
      mem <- ask
      value <- lift $ stToIO $ Memory.load mem address
      trace ("Loaded value " ++ show value) $ lift $ stToIO $ Memory.load mem address
    store address value = DebugEmulator $ do
      trace ("Storing value " ++ show value ++ " to address " ++ show address) $ return ()
      mem <- ask
      lift $ stToIO $ Memory.store mem address value
    clearScreen = DebugEmulator $ do
      trace "Clear screen" $ return ()
      mem <- ask
      lift $ stToIO $ BA.fill (videoMemory mem) False
    drawSprite x y n (Ram address) = DebugEmulator $ do
      trace "drawSprite" $ return ()
      return False -- TODO: undefined
    drawSprite _ _ _ _ = error "Incorrect Address in drawSprite"
    randomWord8 = DebugEmulator $ do
      trace "randomWord8" $ return ()
      mem <- ask
      g <- lift $ stToIO $ readSTRef $ stdGen mem
      let (rnd, g') = randomR (0x0, 0xFF) g
      lift $ stToIO $ writeSTRef (stdGen mem) g'
      return rnd 
    handleEvents = undefined
    waitForKeyPress = DebugEmulator $ do
      trace "waitForKeyPress" $ return ()
      return K0 -- TODO: undefined
    isKeyPressed key = DebugEmulator $ do
      trace ("is key pressed " ++ show key) $ return ()
      mem <- ask
      lift $ stToIO $ readArray (keyState $ eventState mem) key
    sleep = undefined
    isDone = undefined

runDebugEmulator :: DebugEmulator a -> IO a
runDebugEmulator (DebugEmulator reader) = do
    g <- newStdGen
    mem <- stToIO $ Memory.new g
    runReaderT reader mem
