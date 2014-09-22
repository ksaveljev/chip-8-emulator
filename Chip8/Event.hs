module Chip8.Event where

import Data.Ix
import Data.STRef
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST)

data Key = K0 | K1 | K2 | K3
         | K4 | K5 | K6 | K7
         | K8 | K9 | KA | KB
         | KC | KD | KE | KF
         deriving (Enum, Ix, Ord, Eq, Show)

data EventState s = EventState { isQuitTime :: STRef s Bool
                               , lastEventKeyDown :: STRef s (Maybe Key)
                               , keyState :: STUArray s Key Bool
                               }

newEventState :: ST s (EventState s)
newEventState = do
    isQuitTime' <- newSTRef False
    lastEventKeyDown' <- newSTRef Nothing
    keyState' <- newArray (K0, KF) False
    return EventState { isQuitTime = isQuitTime'
                      , lastEventKeyDown = lastEventKeyDown'
                      , keyState = keyState'
                      }

timeToQuit :: EventState s -> ST s ()
timeToQuit es = writeSTRef (isQuitTime es) True

isTimeToQuit :: EventState s -> ST s Bool
isTimeToQuit es = readSTRef (isQuitTime es)

isKeyPressed :: EventState s -> Key -> ST s Bool
isKeyPressed state = readArray (keyState state)

setLastKeyPressed :: EventState s -> Maybe Key -> ST s ()
setLastKeyPressed es = writeSTRef (lastEventKeyDown es)

setKey :: EventState s -> Key -> Bool -> ST s ()
setKey state = writeArray (keyState state)
