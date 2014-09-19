module Chip8.KeyEvent where

import Data.Ix
import Data.STRef
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST)

data Key = K0 | K1 | K2 | K3
         | K4 | K5 | K6 | K7
         | K8 | K9 | KA | KB
         | KC | KD | KE | KF
         deriving (Enum, Ix, Ord, Eq, Show)

data KeyEventState s = KeyEventState { lastEventKeyDown :: STRef s (Maybe Key)
                                     , keyState :: STUArray s Key Bool
                                     }

newKeyEventState :: ST s (KeyEventState s)
newKeyEventState = do
    lastEventKeyDown' <- newSTRef Nothing
    keyState' <- newArray (K0, KF) False
    return KeyEventState { lastEventKeyDown = lastEventKeyDown'
                         , keyState = keyState'
                         }

isKeyPressed :: KeyEventState s -> Key -> ST s Bool
isKeyPressed state = readArray (keyState state)

setKey :: KeyEventState s -> Key -> Bool -> ST s ()
setKey state = writeArray (keyState state)
