{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chip8.Monad.IO where

import Data.STRef (readSTRef, writeSTRef)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Array.ST (readArray)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Control.Applicative
import System.Random (newStdGen, randomR)
import qualified Data.Array.BitArray.ST as BA

import Chip8.Monad
import Chip8.KeyEvent (KeyEventState(..), Key(..))
import Chip8.Memory (Memory(..), Address(..))
import qualified Chip8.Memory as Memory

newtype IOEmulator a = IOEmulator (ReaderT (Memory RealWorld) IO a)
                       deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEmulator IOEmulator where
    load address = IOEmulator $ do
      mem <- ask
      lift $ stToIO $ Memory.load mem address
    store address value = IOEmulator $ do
      mem <- ask
      lift $ stToIO $ Memory.store mem address value
    clearScreen = IOEmulator $ do
      mem <- ask
      lift $ stToIO $ BA.fill (videoMemory mem) False
    drawSprite x y n (Ram address) = IOEmulator $ do
      lift $ putStrLn "HELLO"
      return False -- TODO: undefined
    drawSprite _ _ _ _ = error "Incorrect Address in drawSprite"
    randomWord8 = IOEmulator $ do
      mem <- ask
      g <- lift $ stToIO $ readSTRef $ stdGen mem
      let (rnd, g') = randomR (0x0, 0xFF) g
      lift $ stToIO $ writeSTRef (stdGen mem) g'
      return rnd 
    waitForKeyPress = IOEmulator $ do
      lift $ putStrLn "HI"
      return K0 -- TODO: undefined
    isKeyPressed key = IOEmulator $ do
      mem <- ask
      lift $ stToIO $ readArray (keyState $ keyEventState mem) key

runIOEmulator :: IOEmulator a -> IO a
runIOEmulator (IOEmulator reader) = do
    g <- newStdGen
    mem <- stToIO $ Memory.new g
    runReaderT reader mem
